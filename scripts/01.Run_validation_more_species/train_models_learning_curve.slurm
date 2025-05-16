#!/bin/bash
#SBATCH --job-name=train_model_learning_curve
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --time=2-00:00:00
#SBATCH --mem=5G
##SBATCH --gres=gpu:1
#SBATCH --exclude=toltec-cpu[001-007]

#module load r-rocker-ml-verse/4.2.3+apptainer
module load r-rocker-ml-verse/4.3.2+apptainer
shopt -s expand_aliases

Rscript --no-restore --quiet --no-save batch_flow_more_sp_learning_curve.R


