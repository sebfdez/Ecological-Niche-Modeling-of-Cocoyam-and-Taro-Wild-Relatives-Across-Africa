## Species Environmental Variables/Predictors
## Modified from and cite: https://github.com/mbelitz/Odo_SDM_Rproj and https://github.com/soltislab/BotanyENMWorkshops
## 02-9-2024

library(raster)
library(gtools)
library(ggplot2)
library(plyr)
library(dplyr)
library(rgdal)
library(sp)
library(sf)
library(tmap)
library(tidyr)
library(rnaturalearth)

#set working directory 
setwd("/home/fernandez.se/Cocoyam_Taro/")

## Read in downloaded raw data frame for each species of interest
dir <- "01_DATA"
genus <- "Xanthosoma"
species <- "Xanthosoma_scherberichii" #Include underscore
spec <- gsub("_", " ", species)

############################################################################
############### 1. Read in Accepted Name files and remove NAs ##############
############################################################################

alldf <- read.csv(paste0(dir, "/", genus, "/", "Figures", "/", genus, "_df_combined.csv"))

# Subset alldf with specific species
species_df <- alldf %>% filter(accepted_name %in% c(spec))

# remove NAs
species_df2 <- species_df %>%
  filter(!is.na(long)) %>%
  filter(!is.na(lat))

############################################################################
########################## 2. Define endemic Region  #######################
############################################################################
world <- ne_countries(scale = "medium", returnclass = "sf")

countries <- maps::map(database = "world")

countryList <- world$sovereignt

endemicList <- c("Ecuador")

# Replace any mention of "French Guiana" with "France"
endemicList <- ifelse(endemicList == "French Guiana", "France", endemicList)

# Find elements unique to endemic list
endemicList[!(endemicList %in% countryList)]

############################################################################
#################### 3. Make spatial polygon of endemic region  ############
############################################################################
endemic <- maps::map(database = "world",regions = endemicList,plot = TRUE,fill = FALSE)

endemic <- maps::map(database = "world",regions = endemicList,plot = TRUE,fill = TRUE)

world$endemic <- ifelse(world$name %in% endemicList, "Endemic", "Other")

ggplot(data = world) +
  geom_sf(aes(fill = endemic), color = "gray40") +
  scale_fill_manual(values = c("Endemic" = "black", "Other" = "white")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Global Map with Endemic Countries Filled in Black")

sf_endemic <- world %>%
  filter(name %in% endemicList) %>%
  st_union()  # merge into single polygon

############################################################################
#################### 4. Make spatial dataframe without NAs  ################
############################################################################
# make spatial dataframe
occ_points <- st_as_sf(species_df2, coords = c("long","lat"), crs = 4326)

# Force endemic polygon to match the points CRS
sf_endemic <- st_transform(sf_endemic, st_crs(occ_points))

############################################################################
########################### 5. filter records  #############################
############################################################################
#filters locality based on endemic state, sf object
endemic_polygon <-  st_filter(occ_points, sf_endemic)

#coerce the sf geometry into a dataframe
endemic_df <- do.call(rbind, st_geometry(endemic_polygon)) %>% 
  as_tibble() %>% setNames(c("long","lat"))

#match both lat and long columns to develop an endemic data frame
max_ent <- match_df(species_df2, endemic_df)

### write out endemic maxent file,since maps are done per species, you should append in endemic data per species
write.csv(max_ent,file = file.path(dir, genus, species, 
                                   paste0(species,"_df_endemic.csv")),row.names = FALSE)



# Check if your points are where they should be
ggplot() +
  geom_sf(data = sf_endemic, fill = "lightblue", color = "black") +
  geom_sf(data = occ_points, aes(), color = "red", size = 2) +
  labs(
    title = paste0("Endemic Region + Occurrence Points for ", species),
    subtitle = "Polygon = light blue; Points = red"
  ) +
  theme_minimal()

############################################################################
######################## 6. Merge Everything ###############################
############################################################################  
setwd("/home/fernandez.se/Cocoyam_Taro/")

dir <- "01_DATA"

genus <- "Colocasia" ###############CHANGE HERE#################  

alldf <- list.files(paste0(dir, "/", genus), full.names = TRUE, 
                    recursive = TRUE, include.dirs = FALSE, pattern = "df_endemic.csv")

alldf
alldf <- lapply(alldf, read.csv)
alldf <- do.call(rbind, alldf)

## Select needed columns
alldf <- alldf %>%
  dplyr::select(accepted_name, basisOfRecord, lat, long)

## Save combined dataframe
write.csv(alldf,file = file.path(dir, genus, "Figures", 
                                  paste0(genus,"_df_combined_endemic.csv")),row.names = FALSE)

# Check Number of Records
table(alldf$accepted_name)




