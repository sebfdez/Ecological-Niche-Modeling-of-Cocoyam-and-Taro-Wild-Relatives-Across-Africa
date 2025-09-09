### Future Projections
options(java.parameters = "-Xmx400g")

# Load Packages
library(dplyr)
library(dismo)
library(ENMeval)
library(sp)
library(sf)
library(usdm)
library(viridis)
library(ggplot2)
library(terra)
library(raster)

#setwd
setwd("/blue/soltis/fernandez.se/Cocoyam_Taro")

options(java.parameters = "-Xmx400g")

## Read in downloaded raw data frame for each species of interest
dir <- "/home/fernandez.se/Cocoyam_Taro/01_DATA"
genus <- "Colocasia"

# Load data file
alldf <- read.csv(paste0(dir, "/", genus, "/", "Figures", "/", genus, "_df_combined_endemic.csv"))

############################################################################
########################## 1. Future projections ###########################
############################################################################

# Define the SSP levels and time periods to loop through
ssps <- c("ssp245", "ssp370", "ssp585")
time_periods <- c("2041-2060", "2081-2100")

# Loop through each combination of SSP, and time period
for (ssp in ssps) {
  for (time_period in time_periods) {
      print(paste("Projecting for:", ssp, "Time period:", time_period))
  
      # Load future rasters for the current combination
      biolist <- list.files(paste0("/blue/soltis/fernandez.se/Cocoyam_Taro/01_DATA/Rasters/Africa_Cropped/Future/", time_period, "_", ssp), pattern = "*.tif", full.names = TRUE)
      soillist <- list.files("/blue/soltis/fernandez.se/Cocoyam_Taro/01_DATA/Rasters/Africa_Cropped", pattern = "*_v2.tif", full.names = TRUE)
      slope_water <- list.files("/blue/soltis/fernandez.se/Cocoyam_Taro/01_DATA/Rasters/Africa_Cropped", pattern = "*nded.tif", full.names = TRUE)
      elev <- list.files("/blue/soltis/fernandez.se/Cocoyam_Taro/01_DATA/Rasters/Africa_Cropped", pattern = "*elev.tif", full.names = TRUE)
      
      futureList <- c(biolist, soillist, elev, slope_water)
      
      # Stack rasters
      FutureStack <- raster::stack(futureList)
      
      # Loop through each species
      #for (i in 1:length(unique(alldf$accepted_name))) {
        species <- unique(alldf$accepted_name)[3]
        print(paste("Processing species:", species))
        spec <- gsub(" ", "_", species)
        spec_subset <- dplyr::filter(alldf, accepted_name == species)
      
        # Get names of rasters that were used for species models
        vif_list <- list.files(path = paste0(dir, "/", genus, "/", spec, "/", "species_rasters","/", "00_VIF"), full.names = TRUE)
        specstack <- raster::stack(vif_list)
        layerNames <- names(specstack)
        
        # Get Future rasters that match the layers used in the model
        FutureRasters <- subset(FutureStack, layerNames)
        FutureRasters2 <- terra::rast(FutureRasters)
        print(paste0("Subsetted Future Rasters for ", species))
        
        # Load optimal model Rdata file from ENMeval
        load(paste0(dir, "/", genus, "/", spec, "/", "ENM/", spec, "_optimalSeq_ENM.RData"))
        
        print("Starting terra::predict")
        
        # Project model to Future rasters
        p <- terra::predict(FutureRasters2, mod.seq, filename = paste0(dir, "/", genus, "/", spec, "/", "ENM/" , spec , "_", time_period, "_", ssp, "_ENM_Future_Projection.tif"), na.rm = TRUE, overwrite = TRUE)
        print("terra::predict finished") 
        
        save(p, file = paste0(dir, "/", genus, "/", spec, "/", "ENM/" , spec , "_", time_period, "_", ssp,"_ENM_Future_Projection.RData"))
        print("RData saved")
        
        #LOAD IN FOR MANUAL WORK
        #load(file = paste0(dir, "/", genus, "/", spec, "/", "ENM/", spec, "_ENMevalTEST.RData"))
        
        # Make p plottable 
        p_df <- as.data.frame(p, xy = TRUE)
        world <- map_data("world")
        
        # Plot
        theme_set(theme_bw())
        
        plot <- ggplot(data = world) +
          geom_tile(data = p_df, aes(x = x, y = y, fill = Colocasia_fallax_2041.2060_ssp245_ENM_Future_Projection)) +
          scale_fill_viridis(na.value="transparent", option = "mako", direction = -1)+
          borders("world", colour = "grey70", fill = NA) +  # Add country borders
          geom_point(data = spec_subset, mapping =aes(x = long, y = lat), pch = 19, col='red', cex=0.4, ) +
          xlab("Longitude") +
          ylab("Latitude") +
          coord_sf(xlim = c(-25, 55), ylim = c(-40, 40), expand = FALSE)+
          ggtitle(bquote(italic(.(species)) ~ .(time_period) ~ .(ssp) ~ "Future African Suitability")) +
          labs(fill = "Habitat Suitability")  # Change the legend title
        
        # Define the output file path
        output_file <- paste0(dir, "/", genus, "/", spec, "/", "ENM/" , spec , "_", time_period, "_", ssp, "_ENM_Future_ProjectionTEST.png")
        
        # Save the plot with the desired dimensions (width and height in inches, resolution in dpi)
        ggsave(output_file, plot = plot, width = 12, height = 8, dpi = 300)
        print("Figure saved")
      }
    }
  

