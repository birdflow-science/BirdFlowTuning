## Example
parent_folder <- '/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/norpin/norpin_150km_interval_based_eval_using_migration_transitions/each_transition_evaluation/'
files <- list.files(parent_folder)
res <- list()
for (file in files) {
  data <- readRDS(paste0(parent_folder, file))
  suffix <- sub(".rds", '', sub("^train_each_transition_evaluation_all_combined_", "", file))
  bf <- BirdFlowR::import_birdflow(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/norpin/norpin_150km/{suffix}'))
  cor <- BirdFlowR::distribution_performance(bf, metrics = 'md_traverse_cor', season = 'prebreeding')$md_traverse_cor
  res[[length(res) + 1]] <- list(
    cor = cor,
    model = suffix,
    LL_improvement = sum((data$global_prob_of_the_starting / sum(data$global_prob_of_the_starting))*(data$ll - data$null_ll))
  )
}

res <- do.call(rbind.data.frame, res)
res[res$cor>0.98,] |> dplyr::arrange(-.data[['LL_improvement']])
