## Species Environmental Variables/Predictors
## Modified from and cite: https://github.com/mbelitz/Odo_SDM_Rproj and https://github.com/soltislab/BotanyENMWorkshops
## 03-16-2022

library(raster)
library(gtools)
library(ggplot2)
library(plyr)
library(dplyr)
library(rgdal)
library(sp)
library(sf)
library(viridis)
library(tidyr)
library(multcomp)
library(glue)
library(gridExtra)
library(devtools)
library(ggbiplot)
library(cowplot)
library(multcompView)
library(stringr)


#set working directory 
setwd("/home/fernandez.se/Cocoyam_Taro/")

## Read in downloaded raw data frame for each species of interest
dir <- "01_DATA"
genus <- "Xanthosoma"

############################################################################
################### 1. Read in Cleaned Endemic File ########################
############################################################################

alldf <- read.csv(list.files(path= file.path(dir, genus, "Figures"),
                             pattern= "_df_combined_endemic.csv", full.names = TRUE))

############################################################################
########################## 3. Make map of Taxa #############################
############################################################################

# Separate out Each Unique Species
for(i in 1:length(unique(alldf$accepted_name))){
  species <- unique(alldf$accepted_name)[i]
  print(species)
  spec <- gsub(" ", "_", species)
  species_df <- subset(alldf, accepted_name == species)
  
  ## change species name for usage in filepath
  spec <- gsub(" ", "_", species)
  
  ## Set basemap
  # https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5/topics/borders
  
  #Adjust below to tweak country colors and borders
  world <- borders(database = "world", colour = "#C9DFF2", fill = "#DDE7DE", size=0.1) 
  
  ## Reformat title
  title_expression <- substitute(italic(x) ~ "Endemic Distribution Map", list(x = species))
  
  # extract unique names for morphotypes 
  species_names <- unique(species_df$accepted_name)
  
  #Choose colors for points
  cols <- c("#be0032", "#f3c300"  , "#875692" ,  "#f38400" ,  "#0067a5" ,    "#8db600",  "#848482"  , "#008856" ,  "#e68fac"  ,
            "#2b3d26","#f99379" ,  "#604e97"  , "#f6a600"   ,"#b3446c" ,"#f032e6",  "#dcd300"  , "#882d17" ,  "#8db600"  , "#654522"  ,
            "#e25822", "#a1caf1", "#000", "#FFF", "#800000", "#9A6324", "#fffac8", "#ffd8b1", "#bfef45", "#000075", "#aaffc3",
            "#ff6ec7", "#193C40", "#C9BDF2", "#3889F2")
  
  # Calculate the count for each accepted_name
  species_df_counts <- species_df %>%
    group_by(accepted_name) %>%
    dplyr::summarize(count = n())
  
  # if the above part doesn't work.. restart session
  
  # Merge the counts back to the original dataframe
  species_df <- species_df %>%
    left_join(species_df_counts, by = "accepted_name")
  
  # Sort the data frame by counts in ascending order
  species_df <- species_df %>%
    arrange(desc(count))
  
  # Create a new label combining accepted_name and count
  species_df <- species_df %>%
    mutate(label = paste(accepted_name, " (n = ", species_df_counts$count, ")", sep = ""))
  
  ## Plot 
  world_map <- ggplot() +
    world +
    geom_point(data = as.data.frame(species_df), 
               mapping = aes(x = long, y = lat, color = label, fill = label, shape = basisOfRecord),
               alpha=0.5, size = 0.3) +
    ggtitle(title_expression) +
    xlab("Longitude") +
    ylab("Latitude") +
    
    #Adjust map scale according to distribution
    coord_sf(xlim = c(min(species_df$long) - 1 , max(species_df$long) + 1 ), 
             ylim = c(min(species_df$lat) - 1 , max(species_df$lat) + 1)) +
    
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_shape_manual(values = c(21, 8, 22, 23, 24, 25, 4, 3, 10 )) +
    
    #Adjust below to tweak legend labels
    labs(color ="Species", fill = "Species", shape = "Basis of Record") +
    
    #Adjust below to tweak axis lines and ocean color
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="#C9DFF2"), 
          axis.line = element_line(colour = "black"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.box.just = "left",
          legend.title.position = "top",
          #legend.title = element_text(size = 5), 
          legend.text = element_text(size = 5.5),
          legend.key.size = unit(5, "mm")
    )+
    
    #Adjust below to tweak size of legend elements
    guides(colour = guide_legend(override.aes = list(size = 1), nrow = 4),
           shape  = guide_legend(override.aes = list(size = 1), nrow = 4),
           fill   = guide_legend(override.aes = list(size = 1), nrow = 4))
  
  world_map
  
  #Create directory for maps and other figures
  dir.create(paste0(dir, "/", genus, "/", spec, "/", "Figures"))
  
  #Save for the crop species
  ggsave(paste0(species, "_endemic_distribution.png"), plot = world_map , path = paste0(dir, "/", genus, "/", spec, "/", "Figures"), width = 10, height = 7)

  print(paste0("Map saved for ", species))
  }
################################################################################


