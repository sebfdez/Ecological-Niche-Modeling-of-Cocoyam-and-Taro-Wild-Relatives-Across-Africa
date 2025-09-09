### Cut out Africa Shape on Future Rasters
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
####################### 2. Setup Loops ####################################
############################################################################

ssps <- c("ssp245", "ssp370", "ssp585")
time_periods <- c("2041-2060", "2081-2100")

base_dir <- "/blue/soltis/share/CWR_Proj/02_rasters/FutureCMIP6/ACCESS-CM2"

out_dir <- "/blue/soltis/fernandez.se/Cocoyam_Taro/01_DATA/Rasters/Africa_Cropped/Future"

############################################################################
####################### 3. Get Africa Polygon ##############################
############################################################################

# Download Africa outline
africa <- ne_countries(continent = "Africa", returnclass = "sf")

for (ssp in ssps) {
  for (tp in time_periods) {

    # Build the folder path for this combo
    folder_path <- file.path(base_dir, paste0(tp, "_", ssp))
    
    # List all .tif files in this subfolder
    tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)

    ## Order list using gtools
    climlist <- mixedsort(sort(tif_files))
    
    ### Load rasters
    climstack <- raster::stack(climlist) 
    
    # If needed, transform CRS to match your rasters
    africa <- st_transform(africa, crs = crs(climstack))
    
    # Convert to sp if needed for raster package
    africa_sp <- as(africa, "Spatial")
    
for(j in 1:length(names(climstack))){
  
  # Subset raster layer
  rast <- climstack[[j]]
  
  # Create New Subfolder
  out_subfolder <- file.path(out_dir, paste0(tp, "_", ssp))
  dir.create(out_subfolder, recursive = TRUE, showWarnings = FALSE)
  
  out_name <- file.path(out_subfolder, basename(climlist[j]))
  
  # Crop and mask
  c <- crop(rast, raster::extent(africa_sp))
  c <- mask(c, africa_sp)
  
  writeRaster(c, out_name, format = "Gtiff", overwrite = TRUE)
  
  cat("Processed:", basename(climlist[j]), "\n")
}}}
