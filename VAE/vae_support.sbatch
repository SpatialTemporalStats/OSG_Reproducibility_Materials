#!/bin/bash -l
#SBATCH -J train_bf ## Name of the job
#SBATCH --time=1:00:00

#SBATCH --mail-user=yan.song@kaust.edu.sa
#SBATCH --mail-type=ALL

#SBATCH -n 1
#SBATCH --gpus-per-task=1
#SBATCH --mem=30G
#SBATCH --constraint=a100

module purge
conda activate en_vae

python VAE/Wind_VAE_Perform_Update.py
