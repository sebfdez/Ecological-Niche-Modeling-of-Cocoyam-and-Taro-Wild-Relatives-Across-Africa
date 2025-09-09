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

#setwd
setwd("/blue/soltis/share/CWR_Proj")

## Read in downloaded raw data frame for genus
dir <- "/home/fernandez.se/Cocoyam_Taro/01_DATA"
genus <- "Colocasia"

############################################################################
######################### 1. Read Maxent Ready file ########################
############################################################################
# Load data file
alldf <- read.csv(paste0(dir, "/", genus, "/", "Figures", "/", genus, "_df_combined_endemic.csv"))

############################################################################
################# 2. Pearson's correlation for later use ###################
############################################################################
#for(i in 59:length(unique(alldf$accepted_name))){ #SEB CHANGE HERE
  species <- unique(alldf$accepted_name)[2]
  spec <- gsub(" ", "_", species)
  print(spec)
  
  #Read in layers per species, should be 83
  clippedlist <- list.files(paste0(dir, "/", genus, "/", spec, "/species_rasters/" ), pattern = "*.tif", full.names = TRUE)
  clippedlist2 <- mixedsort(sort(clippedlist))
  clippedstack <- raster::stack(clippedlist2, quick = TRUE)
  print(paste("Rasters read for", species))
  
  ## Start pearsons' - This can then be used after running ENMs to refer to other correlated variables
  corr <- layerStats(clippedstack, 'pearson', na.rm=TRUE)
  print(paste("Correlation has completed for", species))
  
  ### Isolate only the pearson correlation coefficient and take absolute value
  c <- abs(corr$`pearson correlation coefficient`)
  print(paste("Absolute value has completed for", species))
  
  ## write file
  write.csv(c, (paste0(dir, "/", genus, "/", spec, "/", "species_rasters/", "Pearson_correlations.csv") ), row.names = FALSE)
  print(paste("Pearson saved for", species))
  #}
