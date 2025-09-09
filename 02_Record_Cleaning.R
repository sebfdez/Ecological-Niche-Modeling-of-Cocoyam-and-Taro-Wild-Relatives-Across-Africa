## Record Cleaning and spatial thinning 
## Modified from Botany ENM workshop Cite: https://github.com/soltislab/BotanyENMWorkshops

### Load Packages
library(dplyr)
library(tidyr)
library(raster)
library(sp)
library(spatstat)
library(spThin)
library(fields)
library(lubridate)
library(CoordinateCleaner)
library(gatoRs)
library(glue)

##set working directory
setwd("/home/fernandez.se/Cocoyam_Taro/")

dir <- "01_DATA"
genus <- "Xanthosoma" ###############CHANGE HERE#################

############################################################################
####################### 1. Obtain Raw Records Path #########################
############################################################################

# Get the path to each species folder
Genus_Dir <- list.files(paste0(dir,"/", genus), full.names = TRUE)
print(Genus_Dir)

###########################################################################
############################ 2. Clean data #################################
############################################################################

for(i in Genus_Dir){
  ### FOR SKIPPING SPECIES AND FILES
  if(i %in% c("01_DATA/Xanthosoma/Figures", "01_DATA/Colocasia/Figures")) {
    cat(i,"skipped", "\n")
    next
  }
  
   ### Get the full path to the raw record dataframe and read it in
  raw_path <- list.files(path=i, pattern = "_raw.csv", full.names=TRUE)
  print(raw_path)

  species_df <- read.csv(raw_path)

  ### Counts number of observations
  obs_raw <- nrow(species_df)

  ### Counts number of observations from idigbio and gbif
  obs_raw_idigbio <- length(species_df$aggregator[species_df$aggregator == "iDigBio"])
  obs_raw_gbif <-length(species_df$aggregator[species_df$aggregator == "GBIF"])

  ### Creates dataframe with summary information
  obs_raw_df <- data.frame(total = obs_raw, idigbio = obs_raw_idigbio, gbif = obs_raw_gbif)

  ### Gets accepted name from dataframe
  accepted_name <- paste0(species_df$genus[2], "_", species_df$specificEpithet[2])
  write.csv(obs_raw_df, file = paste0(dir, "/", genus, "/" ,accepted_name, "/", accepted_name, "_observations.csv"))
  print(paste(accepted_name, "summary file saved"))

  ### Read in unique name list
  unique_df <- read.csv(file = paste0(dir, "/", genus, "/" , accepted_name, "/", accepted_name, "_names.csv"), header = FALSE)
  species_search <- as.list(unique_df[,1])
  
  ### Compares raw dataframe w/ unique name list and filters out any not in the unique name list
  species_df2 <- gatoRs::taxa_clean(species_df, synonyms.list = species_search, taxa.filter = "exact", accepted.name = accepted_name)
  species_df2 <- species_df2 %>% filter(!is.na(specificEpithet))
  write.csv(species_df2, file = paste0(dir, "/", genus, "/", accepted_name, "/" , accepted_name, "_name_filtered.csv"),row.names = FALSE)
  print(paste(accepted_name, "names filtered and file saved"))

  ### Create new dataframe with adjusted accepted name and less columns
  species_df3 <- species_df2 %>%
    dplyr::select(ID = ID, 
                accepted_name = accepted_name, 
                basisOfRecord = basisOfRecord, 
                coordinateUncertaintyInMeters = coordinateUncertaintyInMeters, 
                informationWithheld = informationWithheld, 
                lat = latitude, 
                long = longitude, 
                year = year, 
                month = month, 
                day = day)

  ### Filters out NAs, Precision, and Remove 00s
  ### https://www.rdocumentation.org/packages/CoordinateCleaner/versions/2.0-20
    species_df3$lat <- round(species_df3$lat, digits = 2)
    species_df3$long <- round(species_df3$long, digits = 2)
    species_df4 <- species_df3 %>%
      filter(long != 0.00) %>%
      filter(lat != 0.00)
    
    ### Checks for ponts near biodiversity institutes
    species_df4 <- cc_inst(species_df4, 
                         lon = "long", 
                         lat = "lat", 
                         species = "accepted_name") 
    
    ###Checks for outliers
    species_df4 <- cc_outl(species_df4, 
                         lon = "long", 
                         lat = "lat", 
                         species = "accepted_name") 

    ### Function to remove duplicates, fix dates, separate into Year/Month/Day,
    ### and remove identical rows
    ### https://github.com/tidyverse/lubridate
      species_df5 <- distinct(species_df4, lat, long, year, month, day, .keep_all = TRUE)
  
    ### Measures distance between occurence points
      nnDm_species <- rdist.earth(as.matrix(data.frame(lon = species_df5$long, lat = species_df5$lat)), miles = FALSE, R = NULL)
      diag(nnDm_species) <- NA
    
    ### Finds minimum distance between occurrence records
      thinpar <- min(nnDm_species[nnDm_species >0], na.rm = TRUE)
      print(thinpar)
      
    ### Removes records that are closer than minimum distance, 
    ### kinda doesn't make sense
      thin_data <- spThin::thin(loc.data =  species_df5,
                                verbose = FALSE,
                                long.col = "long",
                                lat.col = "lat",
                                spec.col = "accepted_name",
                                thin.par = thinpar,
                                reps = 50,
                               locs.thinned.list.return = TRUE,
                                write.files = FALSE)
      
      thin_data <- thin_data[[50]]
      species_df6 <- species_df5[species_df5$lat %in% thin_data$Latitude & species_df5$long %in% thin_data$Longitude, ]


  ### Read in raster file and set resolution
  bio1 <- raster("/blue/soltis/share/CWR_Proj/02_rasters/BioClim/wc2.1_30s_bio_1.tif")
  rasterResolution <- max(res(bio1)) 

  ### Remove a point which nearest neighbor distance is smaller than the resolution size
  while(min(spatstat.geom::nndist(species_df6[, c("long", "lat")])) < rasterResolution){
    nnD_species <- spatstat.geom::nndist(species_df6[, c("long", "lat")])
    species_df6 <- species_df6[-(which(min(nnD_species) == nnD_species) [1]), ] }

    ##Change ones to match
    species_df6$basisOfRecord[species_df6$basisOfRecord == "PreservedSpecimen"] <- "PRESERVED_SPECIMEN"
    species_df6$basisOfRecord[species_df6$basisOfRecord == "Preserved Specimen"] <- "PRESERVED_SPECIMEN"
    species_df6$basisOfRecord[species_df6$basisOfRecord == "preservedspecimen"] <- "PRESERVED_SPECIMEN"
    species_df6$basisOfRecord[species_df6$basisOfRecord == "Preservedspecimen"] <- "PRESERVED_SPECIMEN"
    
    
  ### Write out your freaking cleaned data!!!
  write.csv(species_df6, file = paste0(dir, "/", genus, "/" ,accepted_name, "/", accepted_name, "_cleaned.csv"),row.names = FALSE)

  print(paste("Cleaned records saved for", accepted_name))
  
  ### Appendix stuff
  appendix_df <- species_df %>%
    
    ### Collect useful information about each obs and place into a dataframe  
    dplyr::select(ID = ID, 
                  scientificName = scientificName, 
                  basisOfRecord = basisOfRecord, 
                  occurrenceID = occurrenceID,
                  institutionCode = institutionCode,
                  latitude = latitude, 
                  longitude = longitude, 
                  month = month, 
                  day = day, 
                  year = year)
  
  write.table(appendix_df, file = paste0(dir, "/", genus, "/" ,accepted_name, "/", accepted_name, "_appendix.csv"),col.names = FALSE, append = FALSE, sep = ",")
  }
  
  ############################################################################
  ######################## 3. Merge Everything ###############################
  ############################################################################  
  
  
  
  genus <- "Xanthosoma" ###############CHANGE HERE#################  
  
  alldf <- list.files(paste0(dir, "/", genus), full.names = TRUE, 
                      recursive = TRUE, include.dirs = FALSE, pattern = "_cleaned.csv")
  
  alldf
  alldf <- lapply(alldf, read.csv)
  alldf <- do.call(rbind, alldf)
  
  ## Select needed columns
  alldf <- alldf %>%
    dplyr::select(accepted_name, basisOfRecord, lat, long)
  
  ##check basis column 
  unique_values <- unique(alldf$basisOfRecord)
  
  # Display the unique values and ask for user input to decide which ones to keep
  selected_values <- select.list(unique_values, multiple = TRUE, title = "Select unique basisOfRecord to keep:")
  
  ##Change ones to match
  alldf$basisOfRecord[alldf$basisOfRecord == "PreservedSpecimen"] <- "PRESERVED_SPECIMEN"
  alldf$basisOfRecord[alldf$basisOfRecord == "Preserved Specimen"] <- "PRESERVED_SPECIMEN"
  alldf$basisOfRecord[alldf$basisOfRecord == "preservedspecimen"] <- "PRESERVED_SPECIMEN"
  alldf$basisOfRecord[alldf$basisOfRecord == "Preservedspecimen"] <- "PRESERVED_SPECIMEN"
  
  # Filter the data frame based on the selected values
  alldf2 <- alldf[alldf$basisOfRecord %in% selected_values, ]
  
  ## Save combined dataframe
  write.csv(alldf2,file = file.path(dir, genus, "Figures", 
                                    paste0(genus,"_df_combined.csv")),row.names = FALSE)
  
  ############################################################################
  #################### 5. Fix Format of Basis of Record #####################
  ############################################################################
  ## Read in downloaded raw data frame for each species of interest
  dir <- "01_DATA"
  genus <- "Xanthosoma"
  
  # read in file
  alldf <- read.csv(file = paste0(dir, "/", genus, "/", "Figures", "/", genus, "_df_combined.csv"))
  
  ##check basis column 
  unique(alldf$basisOfRecord)
  
  ##Change ones to match
  alldf$basisOfRecord[alldf$basisOfRecord == "PreservedSpecimen"] <- "PRESERVED_SPECIMEN"
  alldf$basisOfRecord[alldf$basisOfRecord == "Preserved Specimen"] <- "PRESERVED_SPECIMEN"
  alldf$basisOfRecord[alldf$basisOfRecord == "preservedspecimen"] <- "PRESERVED_SPECIMEN"
  alldf$basisOfRecord[alldf$basisOfRecord == "Preservedspecimen"] <- "PRESERVED_SPECIMEN"
  
  ## Save new Maxent.csv
  write.csv(alldf,file = file.path(dir, genus, "Figures", 
                                    paste0(genus,"_df_combined.csv")),row.names = FALSE)
  

  