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
library(rnaturalearth)

## Read in downloaded raw data frame for each species of interest
dir <- "/home/fernandez.se/Cocoyam_Taro/01_DATA"
genus <- "Colocasia"

############################################################################
######################### 1. Read Maxent Ready file ########################
############################################################################

alldf <- read.csv(file = paste0(dir, "/", genus, "/", "Figures", "/", genus, "_df_combined_endemic.csv"))

############################################################################
####### 2. Make your species specific environmental variables ##############
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

# Create Species Training Layers
#for(i in 6:length(unique(alldf$accepted_name))){
  species <- unique(alldf$accepted_name)[2]
  spec <- gsub(" ", "_", species)
  print(paste0("Loaded ", species))
  
  ### FOR ALREADY FINISHED or PROBLEM SPECIES
  #if(species %in% c("Xanthosoma acutilobum", "Xanthosoma aristeguietae", 
   #                 "Xanthosoma acutum", "Xanthosoma brasiliense",
   #                 "Colocasia fontanesii", 
   #                "Colocasia esculenta")) {
   #cat(species,"skipped", "\n")
  #  next
  #}
  
  # Subset species from data frame
  spp_df <-  alldf %>%
    dplyr::filter(accepted_name == species)
  
  print("Subsetted")
  
  # Make spatial
  coordinates(spp_df) <- ~ long + lat
  proj4string(spp_df) <- CRS("+proj=longlat +datum=WGS84")
  
  ## Create alpha hull, if disjunt distrubution use partCount > 1 and fraction < 1
  # From Pascal! https://github.com/ptitle/rangeBuilder/blob/564b4da2ee6409389cc962fb3f2e91ae38d66467/rangeBuilder/R/getDynamicAlphaHull.R
  sphull <- rangeBuilder::getDynamicAlphaHull(x = as.data.frame(spp_df@coords),
                                              coordHeaders = c("long", "lat"),
                                              fraction = 1, # min. fraction of records we want included (e.g. 1 = 100%)
                                              partCount = 1, # number of polygons, 
                                              initialAlpha = 10, # initial alpha size
                                              clipToCoast = "terrestrial", 
                                              #clipToCoast = "no", #use no for stuff with disjunct populations
                                              verbose = TRUE)
  
  print("Alpha hull created")
  
  ### Visualize
  #plot(sphull[[1]], col=transparentColor('gray50', 0.5), border = NA)
  #points(x = spp_df$long, y = spp_df$lat, cex = 0.5, pch = 3)
  
  ### Transform into CRS related to meters
  sphullTrans <- st_transform(sphull[[1]], "+proj=cea +lat_ts=0 +lon_0=0")
  spp_dfTrans <- spTransform(spp_df, "+proj=cea +lat_ts=0 +lon_0")
  
  ### Calculate buffer size
  #### Here we take the 80th quantile of the max distance between points
  spbuffDist <- quantile(x = (apply(spDists(spp_dfTrans), 2, FUN = function(x) sort(x)[2])),
                         probs = 0.80, na.rm = TRUE)
  
  print(paste("Buffer Calculated for", species))
  
  ### Buffer the hull
  spbuffer_m <- st_buffer(x = sphullTrans, dist = spbuffDist, dissolve = TRUE)
  spbuffer <- st_transform(spbuffer_m, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  print("Buffer in meters")
  
  ### Visualize
  # Get bounding box of buffer
  bbox <- st_bbox(spbuffer)
  
  # Optionally expand bbox by e.g. 5% on each side
  expand <- 0.05
  xrange <- bbox$xmax - bbox$xmin
  yrange <- bbox$ymax - bbox$ymin
  
  xlim <- c(bbox$xmin - expand * xrange, bbox$xmax + expand * xrange)
  ylim <- c(bbox$ymin - expand * yrange, bbox$ymax + expand * yrange)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  spp_df_sf <- st_as_sf(spp_df)
  
  hull <- ggplot() +
    geom_sf(data = world, color = "black", fill = "#DDE7DE") +
    geom_sf(data = spbuffer, fill = "pink", color = "black", alpha = 0.5) +
    #borders("world", colour = "#C9DFF2") +
    geom_sf(data = spp_df_sf, aes(), color = "red", size = 1, alpha = 0.7) +
    
    #Adjust map scale according to distribution
    coord_sf(xlim = xlim, ylim = ylim)  +
    labs(
      title = paste0(species, " Buffered Hull")
      ) +
    theme(panel.background=element_rect(fill = "#C9DFF2", colour = "#C9DFF2"))
  
  hull
  
  #Save for the crop species
  if (!dir.exists(paste0(dir, "/", genus, "/", spec, "/", "Figures"))) {
    dir.create(paste0(dir, "/", genus, "/", spec, "/", "Figures"), recursive = TRUE)
  }
  
  ggsave(paste0(species, "_buffered_hull.png"), plot = hull , path = paste0(dir, "/", genus, "/", spec, "/", "Figures"), width = 10, height = 7)
  
  print(paste0("Map saved for ", species))
  
  spbuffer_df <- st_sf(var = 1, spbuffer)
  
  ### Crop and Mask
  # Safely create directory only if it doesn't exist
  if (!dir.exists(paste0(dir, "/", genus, "/", spec, "/", "species_rasters"))) {
    dir.create(paste0(dir, "/", genus, "/", spec, "/", "species_rasters"), recursive = TRUE)
  }
  path <- paste0(dir, "/", genus, "/", spec, "/", "species_rasters", "/")
  end <- ".asc"
  
 print(paste("Writing Rasters for", species))
 
  for(j in 1:length(names(climstack))){
    
    # Subset raster layer
    rast <- climstack[[j]]
    
    # Setup file names
    name <- names(rast)
    out <- paste0(path, name)
    outfile <- paste0(out, end)
    
    # Crop and mask
    c <- crop(rast, raster::extent(spbuffer_df))
    c <- mask(c, spbuffer_df)
    
    # Write raster
    writeRaster(c, outfile, format = "Gtiff", overwrite = TRUE)
  }
  print("Rasters clipped")
#}
