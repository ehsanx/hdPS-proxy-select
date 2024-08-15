#!/bin/bash
#SBATCH --account=st-mekarim-1
#SBATCH --nodes=1
#SBATCH --ntasks=12
#SBATCH --mem=10G
#SBATCH --time=5:00:00
#SBATCH --mail-user=leiyang1@student.ubc.ca
#SBATCH --mail-type=ALL
#SBATCH --output=output_parallel_both.txt
#SBATCH --error=error_parallel_both.txt

module load gcc/9.4.0
module load r/4.4.0

cd $SLURM_SUBMIT_DIR
export R_LIBS=/arc/home/leiyang1/R/x86_64-pc-linux-gnu-library/4.4/

R CMD BATCH parallel_both.R