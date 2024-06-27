#!/bin/bash
#SBATCH --job-name=Simulazione
#SBATCH --partition=tesi
#SBATCH --mail-user=marco.pellizzari.5@studenti.unipd.it
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=8G
#SBATCH --output=/home/pellizzari/SLURM_Simulazioni/out/user_job_%A_%a.out
#SBATCH --error=/home/pellizzari/SLURM_Simulazioni/error/user_error_%A_%a.err
#SBATCH --array=1-100%2


srun singularity exec /home/pellizzari/env/MyCustomImage.sif R CMD BATCH --no-restore /home/pellizzari/SLURM_Simulazioni/SLURM_SimulationsLight.R Simulazioni.Rout
