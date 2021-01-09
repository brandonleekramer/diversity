#!/bin/bash

#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH --job-name = blk_diversity_h1_try1
#SBATCH --time=1-10:00:00
#SBATCH -c 2
#SBATCH --mem=250G

module load goolf R
module load gcc/7.1.0
module load openmpi/3.1.4
module load proj/4.9.2
module load gdal/2.4.0-py36

Rscript /home/kb7hp/git/diversity/src/02_growth-analyses/01_hypothesis1.R