#!/bin/bash
#SBATCH --job-name=CDataDist
#SBATCH --partition=tesi
#SBATCH --mail-user=marco.pellizzari.5@studenti.unipd.it
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=2G
#SBATCH --output=/home/pellizzari/SLURM_Simulazioni/out/user_job_%A.out
#SBATCH --error=/home/pellizzari/SLURM_Simulazioni/error/user_error_%A.err

srun singularity exec /home/pellizzari/env/MyCustomImage.sif R CMD BATCH --no-restore /home/pellizzari/SLURM_Simulazioni/dataset_Feathering.R featherData.Rout
