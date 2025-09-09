# Cocoyam and Taro Environmental Niche Modeling 
Using Ecological Niche Models (ENMs), we are able to identify the factors that influence the habitat suitability of taro and cocoyam crop wild relatives (CWRs) and project high performing models across Africa to analyze shifts in species' current and future habitat suitability.

**01_Raw_Records.R** - downloads occurence records for CWRs from iDigBio and the Global Biodiversity Information Facility (GBIF)

**02_Record_Cleaning.R** - removes records lacking precise spatial coordinates, duplicate records, or occurences near biodiversity institutions

**03_Endemicization.R** - filters to only include points within each species' endemic range

**04_Endemic_Mapping.R** - plots cleaned occurences on map highlighting endemic range 

**05_Accessible_Area.R** - generates an buffered alpha hull around occurence points

**06_VIF.R** - runs a Variable Inflation Factor (VIF) test for each species to remove any variables exhibiting high multicollinearity

**07_Pearson_Corr.R** - calculates pairwise Pearson’s correlation coefficients for the environmental predictor layers

**08_Ecological_Niche_Modeling.R** - creates ENMs for each species using the Maxent algorithm implemented through the ENMeval R package

**09_Projection_Cropping.R** - subsets current global environmental layers to Africa

**10_Nulls.R** - evaluates optimal models against numerous null models

**11_Current_Projection.R** - project current optimal ENMs into Africa

**12_Projection_Cropping.R** - subsets future global environmental layers to Africa

**13_Future_Projection.R** - project current optimal ENMs into Africa using future climate data

**14_Projection_Comparison.R** - compare current and future suitable area to visualize potential change
