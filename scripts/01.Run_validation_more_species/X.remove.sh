#!/bin/bash

# Target date
target_date="2025-05-01"

# Convert to epoch
target_epoch=$(date -d "$target_date" +%s)

# Find all matching folders
for dir in /project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/*/*_150km; do
  # Skip if not a directory
  [ -d "$dir" ] || continue

  # Get last modification time in epoch
  mod_time=$(stat -c %Y "$dir")

  # Compare and delete if older
  if [ "$mod_time" -lt "$target_epoch" ]; then
    echo "Removing $dir"
    rm -rf "$dir"
  fi
done

