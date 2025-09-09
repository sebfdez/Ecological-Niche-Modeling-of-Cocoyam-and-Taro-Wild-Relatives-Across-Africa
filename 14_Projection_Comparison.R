### Projection Comparions
## script by ME Mabry

# Load necessary libraries
library(tidyverse)
library(terra)
library(biomod2)
library(raster)
library(dplyr)
library(ggplot2)
library(maps)
library(viridis)
library(dplyr)

#setwd
setwd("/blue/soltis/share/CWR_Proj")

options(java.parameters = "-Xmx400g")

## Read in downloaded raw data frame for each species of interest
dir <- "/home/fernandez.se/Cocoyam_Taro/01_DATA/"
genus <- "Colocasia"
species <- "Colocasia esculenta"
spec <- gsub(" ", "_", species)

# Load data file
alldf <- read.csv(paste0(dir, "/", genus, "/", "Figures", "/", genus, "_df_combined_endemic.csv"))

# Print the species being processed
print(paste("Processing species:", species))

# Define scenarios and time periods
time_periods <- c("2041-2060", "2081-2100")
ssps <- c("ssp245","ssp370", "ssp585")

#time_period <- c("2041-2060")
#ssp <- c("ssp245")

# Define function to perform comparison and plot for a given species, scenario, and time period
compare_scenarios <- function(species, ssp, time_period) {
  
  print(paste0("Processing: ", species, " ", time_period, " ", ssp))
  
  spec <- gsub(" ", "_", species)
  spec_subset <- dplyr::filter(alldf, accepted_name == species)
  
  # Read in current and future raster files
  current <- raster(paste0(dir, genus, "/", spec, "/ENM/", spec, "_African_Projection.tif"))
  future <- raster(paste0(dir, genus, "/", spec, "/ENM/", spec, "_", time_period, "_", ssp, "_ENM_Future_Projection.tif"))
  
  # Create a binary raster using a cutoff value of 0.7
  current_binary <- calc(current, fun = function(x) ifelse(x >= 0.7, 1, 0))
  future_binary <- calc(future, fun = function(x) ifelse(x >= 0.7, 1, 0))
  
  # make dir
  if (!dir.exists(paste0(dir, genus, "/", spec, "/ENM/ProjectionComparisons/"))) {
    dir.create(paste0(dir, genus, "/", spec, "/ENM/ProjectionComparisons/"), recursive = TRUE)
  }
  
  #Define output file names for binary rasters
  current_output_file <- paste0(dir, genus, "/", spec, "/ENM/ProjectionComparisons/", spec, "_current_binary.asc")
  future_output_file <- paste0(dir, genus, "/", spec, "/ENM/ProjectionComparisons/", spec, "_", time_period, "_", ssp, "_binary.asc")
  
  # Save the current binary raster as an tiff file
  writeRaster(current_binary, filename = current_output_file, format = "Gtiff", overwrite = TRUE)
  
  # Save the future binary raster as an tiff file
  writeRaster(future_binary, filename = future_output_file, format = "Gtiff", overwrite = TRUE)
  
  # Calculate range size differences
  RangeSizeDiff <- BIOMOD_RangeSize(current_binary, future_binary)
  results <- RangeSizeDiff$Compt.By.Models
  
  # Save results to CSV
  write.csv(results, file = paste0(dir, genus, "/", spec, "/ENM/ProjectionComparisons/", spec, "_current_v_", time_period, "_", ssp, "_Comparison.csv"))
  
  # Create a dataframe for plotting
  df <- as.data.frame(RangeSizeDiff$Diff.By.Pixel, xy = TRUE)
  readr::write_csv(df, file = paste0(dir, genus, "/", spec, "/ENM/ProjectionComparisons/", spec, "_current_v_", time_period, "_", ssp, "_Comparison.csv.gz"))
  
  fill <- factor(df[, 3])
  
  # Load the world map data
  world <- map_data("world")
  
  # Extract counts dynamically from the results variable
  loss_count <- results[1]
  stable0_count <- results[2]
  stable1_count <- results[3]
  gain_count <- results[4]
  
  # Generate plot
  plot <- ggplot() +
    # Add the world map as a background layer
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    
    # Overlay the pixel data as a raster layer
    geom_raster(data = df, aes(x = x, y = y, fill = fill)) +
    
    # Set plot labels 
    xlab("Longitude") +
    ylab("Latitude") +
    
    coord_sf(xlim = c(-25, 55), ylim = c(-40, 40), expand = FALSE)+
    
    # Set plot title with species name in italics and dynamic SSP labels
    ggtitle(bquote(italic(.(species)) ~ " Current vs " ~ .(time_period) ~ " (" ~ .(ssp) ~ ") Habitat Suitability")) +
    
    # Customize the color scale for fill with dynamic labels
    scale_fill_viridis_d(na.value = "white", 
                         name="Pixel Change",
                         breaks=c(-2, -1, 0, 1),
                         labels=c(
                           paste0("lost (", loss_count, " pixels)"),
                           paste0("unchanged (", stable1_count, " pixels)"),
                           paste0("not occupied (", stable0_count, " pixels)"),
                           paste0("occupied in the future (", gain_count, " pixels)")
                         )) +
    
    # Customize theme with legend at the bottom in two columns
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(color = "black"),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 8)) +
    guides(fill = guide_legend(ncol = 2))  # Arrange legend in two columns
  
  # Save plot as PNG
  ggsave(paste0(dir, genus, "/", spec, "/ENM/ProjectionComparisons/", spec, "_current_v_", time_period, "_", ssp, "_Comparison.png"), 
         plot = plot, width = 12, height = 8, dpi = 300)
}


## use this loop is running species as variable
  for (ssp in ssps) {
    print(ssp)
    for (time_period in time_periods) {
      print(time_period)
      # Run the comparison function for each combination
      compare_scenarios(species, ssp, time_period)
    }
  }
