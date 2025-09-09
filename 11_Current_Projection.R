### Project ENM to Africa
# CITE: https://github.com/soltislab/BotanyENMWorkshops

# Load Packages
library(tidyverse)
library(raster)
library(gtools)
library(dplyr)
library(dismo)
library(ENMeval)
library(devtools)
#install_github("marlonecobos/kuenm")
#library(kuenm)
library(ggplot2)
library(ggspatial)
library(viridis)
library(maps)

#setwd
setwd("/home/fernandez.se/Cocoyam_Taro/")

options(java.parameters = "-Xmx400g")

## Read in downloaded raw data frame for genus
dir <- "/home/fernandez.se/Cocoyam_Taro/01_DATA"
genus <- "Colocasia"

############################################################################
######################### 1. Read Maxent Ready file ########################
############################################################################
# Load data file
alldf <- read.csv(paste0(dir, "/", genus, "/", "Figures", "/", genus, "_df_combined_endemic.csv"))

############################################################################
############## 2. load African Environmental rasters ########################
############################################################################

climlist <- list.files(paste0(dir, "/Rasters/Africa_Cropped"), pattern = "*.tif", full.names = TRUE)

## Order list using gtools
climlist <- mixedsort(sort(climlist))

### Load rasters
climstack <- raster::stack(climlist) 

############################################################################
################### 3. Project ENMs across Africa ##########################
############################################################################
#for(i in 24:length(unique(alldf$accepted_name))){
  species <- unique(alldf$accepted_name)[2]
  spec <- gsub(" ", "_", species)
  print(spec)
  spec_subset <- dplyr::filter(alldf, accepted_name == species)
  
  ## FOR ALREADY FINISHED or PROBLEM SPECIES
  if(species %in% c("Xanthosoma seideliae", "Xanthosoma diazii")) {
  cat(species,"skipped", "\n")
   next
  }
  
  # Get names of rasters that were used for species models (using vif)
  specstack <- stack(mixedsort(sort(list.files(path=paste0(dir, "/", genus, "/", spec, "/species_rasters/00_VIF"), full.names = TRUE))))
  layerNames <- names(specstack)
  
  # Get Africa rasters which match the layers used in your model by the names
  africaRasters <- subset(climstack, layerNames)
  africaRasters2 <- terra::rast(africaRasters)
  print("Subsetted Africa Rasters")
  
  # Load Rdata file of the optimal model
  load(paste0(dir, "/", genus, "/", spec, "/ENM/", spec, "_optimalSeq_ENM.RData"))
  print("Optimal Model Loaded")
  
  
  # Project model to africa rasters
  p <- terra::predict(africaRasters2, mod.seq, filename = paste0(dir, "/", genus, "/", spec, "/ENM/", spec, "_African_Projection.tif"), na.rm=TRUE, overwrite = TRUE)
  
  p <- raster(paste0(dir, "/", genus, "/", spec, "/ENM/", spec, "_African_Projection.tif"))
  print(paste0(species, " Projection Projected"))
  
  ### Visualize
  p_df <- terra::as.data.frame(p, xy = TRUE)
  world <- map_data("world")

  theme_set(theme_bw())

  plot <- ggplot(data = world) +
    geom_tile(data = p_df, aes(x = x, y = y, fill = lyr1)) +
    scale_fill_viridis(na.value="transparent", option = "mako", direction = -1)+
    borders("world", colour = "grey70", fill = NA) +  # Add country borders
    geom_point(data = spec_subset, mapping =aes(x = long, y = lat), pch = 19, col='red', cex=0.4, ) +
    xlab("Longitude") +
    ylab("Latitude") +
    coord_sf(xlim = c(-25, 55), ylim = c(-40, 40), expand = FALSE)+
    ggtitle(bquote(italic(.(species)) ~ "Africa Suitability"))+
    labs(fill = "Habitat Suitability")  # Change the legend title


  # Define the output file path
  output_file <- paste0(dir, "/", genus, "/", spec, "/ENM/", spec, "_African_Projection.png")

  # Save the plot with the desired dimensions (width and height in inches, resolution in dpi)
  ggsave(output_file, plot = plot, width = 12, height = 8, dpi = 300)
  print(paste0("Current Projection saved for ", species))
#}
