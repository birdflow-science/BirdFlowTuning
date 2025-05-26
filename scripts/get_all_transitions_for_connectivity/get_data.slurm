#!/bin/bash
#SBATCH --job-name=train_model
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --time=2-00:00:00
#SBATCH --mem=5G
##SBATCH --gres=gpu:1
#SBATCH --exclude=toltec-cpu[001-007]
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null

LOG_FILE="output.log"

# Log environment information (optional)
echo "Running on node: $(hostname)" > "$LOG_FILE"
echo "SLURM Job ID: $SLURM_JOB_ID" >>"$LOG_FILE"
echo "SLURM Task ID: $SLURM_TASK_ID" >>"$LOG_FILE"
echo "Requested resources:" >> "$LOG_FILE"
echo "CPUs per task: $SLURM_CPUS_PER_TASK" >> "$LOG_FILE"
echo "Memory: $SLURM_MEM_PER_NODE MB" >> "$LOG_FILE"
echo "Time limit: $SLURM_TIMELIMIT minutes" >> "$LOG_FILE"
echo "Start time: $(date)" >> "$LOG_FILE"
echo "----------------------------------------" >> "$LOG_FILE"

exec >> "$LOG_FILE" 2>&1

module load apptainer/latest
shopt -s expand_aliases
CONTAINER=/work/pi_drsheldon_umass_edu/birdflow_modeling/BirdFlowContainer45/BirdFlowContainer.sif

apptainer exec \
  --bind '' \
  $CONTAINER \
  Rscript --no-restore --quiet --no-save -e "library(BirdFlowPipeline); combine_data_for_all_sp()"


# Capture exit code
EXIT_CODE=$?
if [ $EXIT_CODE -ne 0 ]; then
    echo "----------------------------------------" >> "$LOG_FILE"
    echo "Error: script failed with exit code $EXIT_CODE" >> "$LOG_FILE"
    echo "Check the logs below for details." >> "$LOG_FILE"
    echo "End time: $(date)" >> "$LOG_FILE"
    echo "----------------------------------------" >> "$LOG_FILE"
    exit $EXIT_CODE
fi

echo "----------------------------------------" >> "$LOG_FILE"
echo "Job completed successfully." >> "$LOG_FILE"
echo "End time: $(date)" >> "$LOG_FILE"




