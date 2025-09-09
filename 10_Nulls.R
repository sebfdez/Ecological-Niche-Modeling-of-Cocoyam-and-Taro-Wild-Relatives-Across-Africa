# NULL MODELING #

setwd("/home/fernandez.se/Cocoyam_Taro/")

##################################################################
#################### Load Packages ###############################
##################################################################

library(ENMeval)
library(blockCV)
library(sf)
library(dplyr)
library(gtools)
library(raster)
library(ecospat)
library(ggplot2)

##################################################################
#################### Reading in data for ENMeval #################
##################################################################

## Read in downloaded raw data frame for genus
dir <- "/home/fernandez.se/Cocoyam_Taro/01_DATA"
genus <- "Colocasia"

############################################################################
######################### 1. Read Maxent Ready file ########################
############################################################################
# Load data file
alldf <- read.csv(paste0(dir, "/", genus, "/", "Figures", "/", genus, "_df_combined_endemic.csv"))

#for(i in 4:length(unique(alldf$accepted_name))){
  
  #Selects specific species
  species <- unique(alldf$accepted_name)[2]
  spec <- gsub(" ", "_", species)
  spec_subset <- dplyr::filter(alldf, accepted_name == species)
  print(paste0(species, " Loaded In"))
  
  #Create Directory
  if (!dir.exists(paste0(dir, "/", genus, "/", spec, "/", "ENM/Null"))) {
    dir.create(paste0(dir, "/", genus, "/", spec, "/", "ENM/Null"), recursive = TRUE)
  }
  
  #Reading the optimal model to determine the fc and rm used in null model generation
  load(paste0(dir, "/", genus, "/", spec, "/ENM/", spec, "_optimalSeq_ENM.RData"))
  opt.seq <- read.delim(paste0(dir, "/", genus, "/", spec, "/ENM/Eval_Results/", spec, "_opt_seq.txt"))
  
  # To edit opt.seq and save as
  #opt.seq <- opt.seq[-c(2), ] #Replace number with row you want to remove
  #write.table(opt.seq, file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_opt_seq.txt"), sep = "\t") 
  
  fc <- opt.seq$fc
  rm <- opt.seq$rm
  
  #Loads in ENM eval1 variable
  load(paste0(dir, "/", genus, "/", spec, "/ENM/", spec, "_ENMeval.RData"))
  
  #Creates 1,000 null models
  spec.mod.null <- ENMnulls(eval1, mod.settings = list(fc = fc, rm = rm), no.iter = 150) 
  
  #Inspecting results
  null.results(spec.mod.null) %>% head()
  Null_Comparison_Results <- null.emp.results(spec.mod.null)
  write.csv(Null_Comparison_Results, file= paste0(dir, "/", genus, "/", spec, "/", "ENM/Null/", spec, "_Null_Comparison_Results.csv"), row.names = FALSE) 
  
  
  #Plotting & saving the results in a histogram
  spec.null <- evalplot.nulls(spec.mod.null, stats = c("or.10p", "auc.val"), plot.type = "histogram")
  ggsave(filename = paste0(spec, "_Null_histogram.png"), plot = spec.null, path = paste0(dir, "/", genus, "/", spec, "/", "ENM/Null/"), height = 12, width = 13)
#}