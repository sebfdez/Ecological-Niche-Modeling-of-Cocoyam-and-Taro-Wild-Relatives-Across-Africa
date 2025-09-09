#!/bin/bash

#SBATCH --job-name=Sh_Pearson_Esculenta                                # Job name
#SBATCH --output=%x_%j.out                                # Standard output and error log
#SBATCH --mail-type=ALL
#SBATCH --mail-user=fernandez.se@ufl.edu
#SBATCH --nodes=1                                         # Run all processes on a single node
#SBATCH --ntasks=1                                        # Run a single task
#SBATCH --cpus-per-task=1                                 # Number of CPU cores per task
#SBATCH --mem=40gb                                        # Job memory request
#SBATCH --time=00-48:00:00                                 # Time limit days-hrs:min:sec
#SBATCH --qos=soltis-b

pwd; hostname; date

#load modules
module load R/4.2

Rscript --vanilla /blue/soltis/fernandez.se/Cocoyam_Taro/02_SCRIPTS/07_Pearson_Corr.R