### Cut out Africa Shape on Rasters
# CITE: https://github.com/soltislab/BotanyENMWorkshops

# Load Packages
library(tidyverse)
library(raster)
library(gtools)
library(dplyr)
library(dismo)
library(ENMeval)
library(devtools)
library(ggplot2)
library(ggspatial)
library(viridis)
library(maps)
library(rnaturalearth)
library(sf)
library(raster)


#setwd
setwd("/blue/soltis/share/CWR_Proj")

options(java.parameters = "-Xmx400g")

## Read in downloaded raw data frame for each species of interest
dir <- "/home/fernandez.se/Cocoyam_Taro/01_DATA"

############################################################################
############## 2. load Global Environmental rasters ########################
############################################################################
biolist <- list.files("/blue/soltis/share/CWR_Proj/02_rasters/BioClim", pattern = "*.tif", full.names = TRUE)
soillist <- list.files("/blue/soltis/share/CWR_Proj/02_rasters/SoilGrids", pattern = "*_v2.tif", full.names = TRUE)
slope <- list.files("/blue/soltis/fernandez.se/Cocoyam_Taro/01_DATA/Rasters/Slope", pattern = "*nded.tif", full.names = TRUE)
water_dist <- list.files("/blue/soltis/fernandez.se/Cocoyam_Taro/01_DATA/Rasters/Water_Dist", pattern = "*nded.tif", full.names = TRUE)

climlist <- c(biolist, soillist, slope, water_dist)

## Order list using gtools
climlist <- mixedsort(sort(climlist))

### Load rasters
climstack <- raster::stack(climlist) 

############################################################################
####################### 3. Get Africa Polygon ##############################
############################################################################

# Download Africa outline
africa <- ne_countries(continent = "Africa", returnclass = "sf")

# If needed, transform CRS to match your rasters
africa <- st_transform(africa, crs = crs(climstack))

# Convert to sp if needed for raster package
africa_sp <- as(africa, "Spatial")

path <- paste0(dir, "/Rasters/Africa_Cropped/")
end <- ".asc"

for(j in 1:length(names(climstack))){
  
  # Subset raster layer
  rast <- climstack[[j]]
  
  # Setup file names
  name <- names(rast)
  out <- paste0(path, name)
  outfile <- paste0(out, end)
  
  # Crop and mask
  c <- crop(rast, raster::extent(africa_sp))
  c <- mask(c, africa_sp)
  
  # Write raster
  writeRaster(c, outfile, format = "Gtiff", overwrite = TRUE)
}
