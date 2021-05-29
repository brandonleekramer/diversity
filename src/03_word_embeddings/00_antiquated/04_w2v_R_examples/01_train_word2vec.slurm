#!/bin/bash

#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH --job-name=train_embeddings_1520
#SBATCH --time=1-100:00:00
#SBATCH -c 32
#SBATCH --mem=200G

module purge
ml gcc/7.1.0 openmpi/3.1.4 R/4.0.0

## /home/kb7hp/git/diversity/src/04_word_embeddings/
## 01_train_word2vec.slurm

Rscript 01_train_word2vec.R
