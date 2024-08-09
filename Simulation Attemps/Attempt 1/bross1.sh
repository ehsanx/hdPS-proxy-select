#!/bin/bash
#SBATCH --account=st-mekarim-1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=0
#SBATCH --time=0-20:00:00
#SBATCH --mail-user=leiyang1@student.ubc.ca
#SBATCH --mail-type=ALL
module load gcc/9.4.0
module load r/4.4.0
export R_LIBS=/home/leiyang1/R/x86_64-pc-linux-gnu-library/4.4
R CMD BATCH bross1.R