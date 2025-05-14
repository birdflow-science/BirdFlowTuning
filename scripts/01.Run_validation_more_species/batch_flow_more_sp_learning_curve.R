##
library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.

unique_names <- c(
  gsub('.rds','',list.files('/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/motus/rds/')),
  gsub('.rds','',list.files('/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/banding/rds/'))
) |> unique()


#### Batch fit
unique_names <- unique_names


sp = 'amewoo'
for (frac in c(0.01, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1)) {
  print(sp)
  sp_output_path <- paste0('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric','/',sp)
  if (!dir.exists(sp_output_path)){dir.create(sp_output_path, recursive = TRUE)}
  
  ## Load
  file1 <- glue::glue("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_interval_based_eval_using_migration_transitions_frac{frac}/eval_metrics_train_distance_metric_all_combined.rds")
  file2 <- glue::glue("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_interval_based_eval_using_migration_transitions_frac{frac}/eval_metrics_test_distance_metric_all_combined.rds")
  if (file.exists(file1) & file.exists(file2)){
    rds1 <- readRDS(file1)
    rds2 <- readRDS(file2)
    pass <- TRUE
    for (target in c('traverse_cor_st','weighted_energy_improvement','weighted_mean_win_distance_fraction','synth_routes_prebreeding_migration_straightness',
                     'end_traverse_cor_whole_year')){
      if (!((target %in% names(rds1)) & (target %in% names(rds2)))){
        pass <- FALSE
      }
    }
    
    if (pass){
      print(glue::glue('Already finished for {sp}'))
      next
    } else {
      print(glue::glue('The existing files not passed for {sp}; Rerun.'))
    }
  }
  
  tryCatch({
    batch_flow(sp, 
               gpu_ram = 10,
               training_subsample_frac = frac,
               hdf_path = sp_output_path,
               base_output_path = sp_output_path,
               model_selection = 'distance_metric',
               suffix=glue::glue('interval_based_eval_using_migration_transitions_frac{frac}'),
               # skip_quality_checks=TRUE,
               # min_season_quality = 1
               # fit_only=TRUE
    )
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  })
  
}



