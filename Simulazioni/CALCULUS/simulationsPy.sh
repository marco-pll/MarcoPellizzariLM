#!/bin/bash
#SBATCH --job-name=S12
#SBATCH --partition=tesi
#SBATCH --mail-user=marco.pellizzari.5@studenti.unipd.it
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=2G
#SBATCH --output=/home/pellizzari/SLURM_Simulazioni/out/user_job_%A.out
#SBATCH --error=/home/pellizzari/SLURM_Simulazioni/error/user_error_%A.err
#SBATCH --array=1-100%8

srun singularity exec --env-file /home/pellizzari/env/env_file /home/pellizzari/env/pythonImageCPU.sif python SLURM_NNLight.py $SLURM_ARRAY_TASK_ID
