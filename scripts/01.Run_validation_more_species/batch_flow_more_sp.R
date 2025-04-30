# ## Re-install BirdFlow & BirdFlowPipeline
# for (package in c('BirdFlowR','BirdFlowPipeline')){
#   if ("BirdFlowR" %in% installed.packages()) {
#     remove.packages(package)
#     # detach("package:BirdFlowR", unload = TRUE)
#   } else {
#     cat(paste0(package, " is not installed.\n"))
#   }
# }

# devtools::document("/home/yc85_illinois_edu/BirdFlowR")
# devtools::document("/home/yc85_illinois_edu/BirdFlowPipeline")
devtools::install_local("/home/yc85_illinois_edu/BirdFlowR", force = T, dependencies = FALSE) # if the BirdFlowR is updated, we need to reinstall it, so that i can be used in BirdFlowPipeline!
devtools::install_local("/home/yc85_illinois_edu/BirdFlowPipeline", force = T, dependencies = FALSE)

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
unique_names <- unique_names #c('ovenbi1', 'paibun', 'whimbr', 'lobcur', 'osprey', 'tunswa', 'bkbplo') #c('lobcur') #c('amewoo', 'buwtea', 'swahaw', 'brwhaw', 'woothr') #, 'lobcur',
# unique_names <- c('yelwar')
for (sp in unique_names){
  print(sp)
  sp_output_path <- paste0('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric','/',sp)
  if (!dir.exists(sp_output_path)){dir.create(sp_output_path, recursive = TRUE)}

  ## Load
  file1 <- glue::glue("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_interval_based_eval_using_migration_transitions/eval_metrics_train_distance_metric_all_combined.rds")
  file2 <- glue::glue("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_interval_based_eval_using_migration_transitions/eval_metrics_test_distance_metric_all_combined.rds")
  if (file.exists(file1) & file.exists(file2)){
    rds1 <- readRDS(file1)
    rds2 <- readRDS(file2)
    pass <- TRUE
    for (target in c('weighted_energy_improvement','weighted_mean_win_distance_fraction','synth_routes_prebreeding_migration_straightness',
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
               hdf_path = sp_output_path,
               base_output_path = sp_output_path,
               model_selection = 'distance_metric',
               suffix='interval_based_eval_using_migration_transitions',
               skip_quality_checks=FALSE,
               min_season_quality = 3
               # fit_only=TRUE
               )
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  })
}

