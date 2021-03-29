#!/bin/bash

#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH --job-name=diversity_h1_date
#SBATCH --time=1-24:00:00
#SBATCH -c 32
#SBATCH --mem=200G

module purge
ml gcc/7.1.0 openmpi/3.1.4 R/4.0.0

Rscript 01_hypothesis1.R