
## Download Record Data
## Modified from Botany ENM workshop Cite: https://github.com/soltislab/BotanyENMWorkshops
### 2021 06 24

### Load packages 
library(dplyr) 
library(tidyr) 
library(plyr) 
library(spocc) 
library(ridigbio) 
library(tibble) 
library(gatoRs)


### setwd 
setwd("/home/fernandez.se/Cocoyam_Taro/")

############################################################################
########################### 1. Make species lists ##########################
############################################################################
C_list <- c("Colocasia affinis",
                "Colocasia esculenta",
                "Colocasia fallax",
                "Colocasia fontanesii")

X_list <- c("Xanthosoma acutilobum",
            "Xanthosoma brevispathaceum", 
            "Xanthosoma eggersii", 
            "Xanthosoma robustum", 
            "Xanthosoma undipes",
            "Xanthosoma sagittifolium",
            "Xanthosoma viviparum",
            "Xanthosoma syngoniifolium",
            "Xanthosoma brasiliense",
            "Xanthosoma granvillei",
            "Xanthosoma helleborifolium",
            "Xanthosoma pubescens",
            "Xanthosoma dealbatum",
            "Xanthosoma mexicanum",
            "Xanthosoma daguense",
            "Xanthosoma acutum",
            "Xanthosoma conspurcatum",
            "Xanthosoma poeppigii",
            "Xanthosoma wendlandii",
            "Xanthosoma cubense",
            "Xanthosoma pentaphyllum",
            "Xanthosoma striatipes",
            "Xanthosoma hylaeae",
            "Xanthosoma trichophyllum",
            "Xanthosoma aristeguietae",
            "Xanthosoma maximiliani",
            "Xanthosoma plowmanii",
            "Xanthosoma weeksii",
            "Xanthosoma hannoniae",
            "Xanthosoma taioba",
            "Xanthosoma acutilobum",
            "Xanthosoma crassilaminum",
            "Xanthosoma crassinervium",
            "Xanthosoma purpureomaculatum",
            "Xanthosoma reticulatum")

############################################################################
################## 2. Make genus variable and directory######################
############################################################################
# CHANGE
Genus <- "Xanthosoma"

dir <- "/blue/soltis/fernandez.se/Cocoyam_Taro/01_DATA/"

############################################################################
###### 3. use spocc_combine to pull data from iDigBio, BISON, and GBIF #####
############################################################################
path <- paste0(dir, Genus, "/")
end <- ".csv"

# change the list variable below depending on which crop is being downloaded
for(i in X_list){
  name <-i
  print(name)
  
  name2 <- gsub(' ', '_',name)
  print(name2)
  
  # Create the full path to the species subfolder
  species_path <- paste0(path, name2, "/")
  
  # Ensure the species subfolder exists
  dir.create(species_path, showWarnings = FALSE)
  
  out <- paste0(species_path, name2)
  #print(out)
  
  #add Day, month, year to the file name
  outfile <- paste0(out,"_05_15_2025_raw", end)
  #print(outfile)
  
  raw_df <- gators_download(synonyms.list = name, 
                  write.file = TRUE, 
                  filename = outfile, 
                  gbif.match = "fuzzy",
                  idigbio.filter = TRUE)
  
  # Identify and write list of unique names
  #uniquenames <- data.frame(unique(raw_df$scientificName))
  #write.csv(uniquenames, paste0(species_path, "/", name2, "_names.csv"), row.names = FALSE)
  print(paste("Saved data for:", name2))
}
  
