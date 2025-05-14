for i in $(<names.txt)
do
	{
		target_file1_path="/project/pi_drsheldon_umass_edu/birdflow/data_for_connectivity_calculation/${i}/${i}_150km_transition_data/all_ground_truth_transitions_df_filtered_transitions_between_breeding_and_nonbreeding.rds"
#		if [ -f "$target_file1_path" ]; then
#			echo "ALL FINISH for ${i}"
#			continue
#		fi
		sbatch --job-name=${i} get_data.slurm ${i}
	}
done
