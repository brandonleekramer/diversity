#!/bin/bash

#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH --job-name=diversity_h1_h3
#SBATCH --time=1-24:00:00
#SBATCH -c 2
#SBATCH --mem=250G

module purge
ml gcc/7.1.0 openmpi/3.1.4 R/4.0.0

Rscript 01_hypothesis1.R
Rscript 02_hypothesis2.R
Rscript 03_hypothesis3a.R
Rscript 03_hypothesis3b.R