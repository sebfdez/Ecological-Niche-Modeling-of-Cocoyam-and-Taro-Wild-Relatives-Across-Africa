## Species Environmental Variables/Predictors
## Modified from and cite: https://github.com/mbelitz/Odo_SDM_Rproj and https://github.com/soltislab/BotanyENMWorkshops
## 03-16-2022

library(raster)
library(gtools)
library(dplyr)
library(rgdal)
library(sp)
library(rangeBuilder)
library(sf)
library(caret)
library(usdm)
library(dismo)
library(stringr)
library(rJava)
library(viridis)

#set working directory 
setwd("/blue/soltis/share/CWR_Proj")

## Read in downloaded raw data frame for each species of interest
dir <- "/home/fernandez.se/Cocoyam_Taro/01_DATA"
genus <- "Colocasia"

############################################################################
######################### 1. Read Maxent Ready file ########################
############################################################################

# Load data file
alldf <- read.csv(paste0(dir, "/", genus, "/", "Figures", "/", genus, "_df_combined_endemic.csv"))

############################################################################
####################### 2. Variable selection ##############################
############################################################################
# Select layers for MaxEnt
## We only want to include layers that are not highly correlated.
## To assess which layers we will include, we will use Variable inflation factors (VIFs) 
## VIF can detect for multicollinearity in a set of multiple regression variables. 
### tutorial which was followed https://rstudio-pubs-static.s3.amazonaws.com/300995_e6e0edf09915431480ed089c3a3e0ff3.html

#for(i in 1:length(unique(alldf$accepted_name))){
  species <- unique(alldf$accepted_name)[2]
  print(species)
  
  ### FOR ALREADY FINISHED or PROBLEM SPECIES
 # if(species %in% c("Xanthosoma brasiliense", "Xanthosoma cubense", "Xanthosoma taioba",
#                    "Colocasia fontanesii", "Colocasia affinis",  
 #                   "Colocasia esculenta")) {
  #  cat(species,"skipped", "\n")
   # next
  #}
  
  ## species name with underscore
  spec <- gsub(" ", "_", species)
  
  ##create dir
  dir.create(paste0(dir, "/", genus,"/", spec,"/", "species_rasters", "/", "00_VIF"), recursive = TRUE)
  
  #### Stack layers for each species (Should have 83)
  clippedlist <- list.files(paste0(dir, "/", genus, "/", spec, "/", "species_rasters" ), pattern = "*.tif", full.names = TRUE)
  clippedlist2 <- mixedsort(sort(clippedlist))
  clippedstack <- raster::stack(clippedlist2)
  clippedstack2 <- terra::rast(clippedstack) # use if you have an error
  print(paste("Rasters stacked for", species))
  
  #calculate VIFs using a threshold of 10
  stepstack <- usdm::vifstep(clippedstack2, th=10)
  print(paste("VIF calculated for", species))
  
  # exclude the collinear variables that were identified in the previous step
  v2 <- exclude(clippedstack,stepstack)
  
  print(paste("Variables Excluded for", species))
  
  ## finally copy the layers we want to a new folder!
  print(paste("Starting to copy the layers for", species))
  
  ## transfer files into directory per species per method
  for(i in 1:length(names(v2))){
    name <- names(v2)[i]
    print(name)
    
    from <- paste0(dir, "/", genus, "/", spec, "/", "species_rasters","/", name, ".tif")
    to <- paste0(dir, "/", genus, "/", spec, "/", "species_rasters","/", "00_VIF", "/", name, ".tif")
    
    file.copy(from, to,
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
  }
#}




