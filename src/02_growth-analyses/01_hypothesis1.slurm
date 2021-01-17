#!/bin/bash

#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH --job-name = blk_diversity_h1
#SBATCH --time=1-24:00:00
#SBATCH -c 2
#SBATCH --mem=250G

module purge
ml gcc/7.1.0 openmpi/3.1.4 R/4.0.0

Rscript 01_hypothesis1.R
Rscript 01_hypothesis1_combine.R