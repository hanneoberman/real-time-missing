#!/bin/bash
#SBATCH --job-name=SIG
#SBATCH --time=96:00:00
#SBATCH --mem-per-cpu=12G
#SBATCH --ntasks-per-node=1
#SBATCH --nodes=1
#SBATCH --array=1-1000
#SBATCH --mail-type=FAIL,END
#SBATCH --mail-user=h.i.oberman@uu.nl
module load R/3.5.1
R CMD BATCH simulation.R