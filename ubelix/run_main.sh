#!/bin/bash
#SBATCH --job-name='covid19_da'
#SBATCH --partition=all
#SBATCH --time=48:00:00

module load R
Rscript ../main.R
