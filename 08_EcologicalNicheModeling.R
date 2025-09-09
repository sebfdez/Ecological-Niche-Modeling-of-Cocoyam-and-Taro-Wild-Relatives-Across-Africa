## Ecological Niche Modeling with ENMevaluate
## CITE: https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0.0-vignette.html and https://github.com/soltislab/BotanyENMWorkshops

# Set up java memory 
options(java.parameters = "-Xmx16G") # increase memory that can be used ; 10240m = 10G

# Load Packages
library(raster)
library(gtools)
library(dplyr)
library(dismo)
library(ENMeval)
library(ggplot2)
library(viridis)
library(devtools)
library(rJava)

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
############################### 2. Run ENMs ################################
############################################################################

#for(i in 61:length(unique(alldf$accepted_name))){
species <- unique(alldf$accepted_name)[2]
spec <- gsub(" ", "_", species)
spec_subset <- dplyr::filter(alldf, accepted_name == species)
print(paste(species, "loaded in"))

if (!dir.exists(paste0(dir, "/", genus, "/", spec, "/", "ENM"))) {
  dir.create(paste0(dir, "/", genus, "/", spec, "/", "ENM"), recursive = TRUE)
}

# Read in species specific VIF selected enviromental data
specstack <- stack(mixedsort(sort(list.files(path=paste0(dir, "/", genus, "/", spec, "/", "species_rasters","/", "00_VIF"), pattern =  ".tif", full.names = TRUE))))
projection(specstack) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Verify that all layers are non-null and aligned
if (any(is.null(raster::values(specstack)))) {
  next("One or more layers in specstack is NULL or misaligned, skipping species")
}

print(paste(species, "VIF layers loaded in"))

# Set up and run maxent models, for description for args: https://groups.google.com/g/maxent/c/yRBlvZ1_9rQ
# Set up an addtional filter for running datasets with less than 10 samples using jackknife partitions (https://doi.org/10.1111/geb.13504)
# Script was originally run with incorrect "<", flip before running again
if (length(spec_subset$accepted_name) < 50) {
  eval1 <- ENMeval::ENMevaluate(occs = spec_subset[, c("long", "lat")],
                                envs = specstack,
                                tune.args = list(fc = c("L","LQ","LQH","H", "LH", "Q", "QH"), rm = 0.5:5),# test the feature classes L = linear and Q = quadratic
                                partitions = "block", 
                                n.bg = 10000,
                                algorithm = 'maxent.jar')
  
} else {
  
  eval1 <- ENMeval::ENMevaluate(occs = spec_subset[, c("long", "lat")],
                                envs = specstack,
                                tune.args = list(fc = c("L","LQ","LQH","H", "LH", "Q", "QH"), rm = 0.5:5),
                                partitions = "jackknife", 
                                n.bg = 500, 
                                algorithm = 'maxent.jar') 
  
}

# Save enm as rdata object
save(eval1, file = paste0(dir, "/", genus, "/", spec, "/", "ENM/", spec, "_ENMeval.RData"))

# LOAD IN FOR MANUAL WORK
#load(file = paste0(dir, "/", genus, "/", spec, "/", "ENM/", spec, "_ENMeval.RData"))

### ENMevaluate results

## Create Directory for storing results
if (!dir.exists(paste0(dir, "/", genus, "/", spec, "/", "ENM/", "Eval_Results"))) {
  dir.create(paste0(dir, "/", genus, "/", spec, "/", "ENM/", "Eval_Results"), recursive = TRUE)
}

## Overall results
results <- eval.results(eval1)
write.table(results, file =  paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_results.txt"), sep = "\t")

## Visualize all models
maps <- eval1@predictions
pdf(file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_ENMresults.pdf"), width = 24, height = 24)
raster::plot(maps[[1:20]], nc=4)
if (nlayers(maps) >= 21) {
  end_layer <- min(40, nlayers(maps))
  raster::plot(maps[[21:end_layer]], nc = 4)
}
dev.off()

## Calculate niche overlap between models to determine similarity between models
mod_overlap <- calc.niche.overlap(eval1@predictions, overlapStat = "D")
write.table(mod_overlap, file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_niche_overlap.txt"), sep = "\t")

## write results for each partition
partition_results <- eval.results.partitions(eval1)
write.table(partition_results, file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_partition_results.txt"), sep = "\t")

## write file that holds environmental values for each occurrence data point
occurance_env_values <- eval.occs(eval1)
write.table(occurance_env_values, file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_occurance_env_values.txt"), sep = "\t")

## write file that holds environmental values for each background data point
background_env_values <- eval.bg(eval1)
write.table(background_env_values, file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_background_env_values.txt"), sep = "\t")

### Visualizing tuning results
evalplot.stats(e = eval1, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm",
               dodge = 0.5)
ggsave(paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_model_comparison.pdf"))

## Model Selection

# Select model based on minimum AIC, omission rate, and largest AUC
opt.seq <- results %>% 
  # dplyr::filter(auc.train != 0.5) %>% #for any species with blank opt model, choose column to remove the visually incorrect models
  dplyr::filter(!is.na(AICc))%>% #any model with an NA for AICc means there are more parameters than observations
  dplyr::filter(AICc == min(AICc))%>%
  dplyr::filter(or.10p.avg !=0)  %>%#exclude any resulting zeroes
  dplyr::filter(or.10p.avg == min(or.10p.avg))%>% #pick model(s) with minimum values
  dplyr::filter(auc.val.avg == max(auc.val.avg)) #pick model with max auc, if more than one min or10p score

write.table(opt.seq, file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_opt_seq.txt"), sep = "\t") 

# Skips species if two or more optimal models are chosen
if (nrow(opt.seq) > 1) {
  cat(species,"skipped, manually choose optimal model", "\n")
    next
}

print(paste0(species, " optimal model tuning determined"))

# USE TO MANUALLY SELECT OPTIMAL MODELS
#opt.seq <- opt.seq[-c(2), ] #Replace number with row you want to remove

## We can select a single model from the ENMevaluation object using the tune.args of our optimal model.
mod.seq <- eval.models(eval1)[[opt.seq$tune.args]]

variable_importance <- mod.seq@results
write.table(variable_importance, file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec,  "_variable_importance.txt"), sep = "\t")

## Look at variable contribution
pdf(file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_variable_contribution.pdf"), width = 12, height = 24)
plot(mod.seq)
dev.off()

## Look at the response curves
pdf(file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_response_curves.pdf"), width = 24, height = 24)
dismo::response(mod.seq)
dev.off()

## Save mod.seq as rdata object
save(mod.seq, file = paste0(dir, "/", genus, "/", spec, "/", "ENM/", spec, "_optimalSeq_ENM.RData"))

## We can select the model predictions for our optimal model the same way we did for the model object above.
pred.seq <- eval.predictions(eval1)[[opt.seq$tune.args]]

pdf(file = paste0(dir, "/", genus, "/", spec, "/", "ENM/Eval_Results/", spec, "_optimal_model_prediction.pdf"))
plot(pred.seq)
dev.off()

#}
