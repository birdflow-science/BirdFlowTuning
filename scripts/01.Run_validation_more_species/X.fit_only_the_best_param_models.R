library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

best_models <- read.csv('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/best_model_set/best_model_summaries/best_models_byST098_and_LL.csv')
# sp = 'amewoo'

for (file in Sys.glob('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/best_model_set/models/*_150km')){
  model_ids <- sub(".*/([^/]+)_150km$", "\\1", file)
  print('--------------------------------')
  print(model_ids)
  print(list.files(file))
}

get_args <- function(params) {
  grid_search_type <- params$grid_search_type
  grid_search_list <- params$grid_search_list
  hdf_dir <- params$hdf_dir
  species <- params$species
  res <- params$res
  ebirdst_year <- params$ebirdst_year
  stopifnot(!is.null(grid_search_type) && grid_search_type %in% c('old', 'new'))
  # base df without grid search parameters
  orig <- data.frame(
    dir = hdf_dir,
    species = species,
    res = res,
    ebirdst_year = ebirdst_year
  )
  orig$id <- seq_len(nrow(orig))
  grid_search_list$id <- orig$id
  df <- expand.grid(grid_search_list)
  
  # if the grid search type is new, calculate dist_weight and ent_weight
  if (grid_search_type == "new"){
    xy <- refactor_hyperparams(df$de_ratio, df$obs_prop)
    df$dist_weight <- xy$dist_weight
    df$ent_weight <- xy$ent_weight
    
    df$de_ratio <- NULL
    df$obs_prop <- NULL
  }
  
  args <- dplyr::left_join(orig, df, by = "id")
  args$id <- NULL
  return(args)
}

res <- list()
for (idx in 1:nrow(best_models)) {
  print(idx)
  # idx <- as.integer(rownames(best_models[best_models$sp=='amewig',]))
  sp <- best_models[idx, 'sp']
  sp_output_path <- paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/best_model_set/models')
  
  ####
  params <- set_pipeline_params(sp, 
                                gpu_ram = 10,
                                hdf_path = sp_output_path,
                                base_output_path = sp_output_path,
                                model_selection = 'distance_metric',
                                suffix='interval_based_eval_using_migration_transitions')
  params <- preprocess_species_wrapper(params)
  
  ####
  modelfit_args_df <- get_args(params)
  print(paste0('Using py script: ', BirdFlowPipeline:::the$python_repo_path))
  best_modelfit_args_df <- modelfit_args_df[(abs(modelfit_args_df$ent_weight - best_models[idx, 'ent']) < 1e-6) &
                     (abs(modelfit_args_df$dist_weight - best_models[idx, 'dist']) < 1e-6) &
                     (abs(modelfit_args_df$dist_pow - best_models[idx, 'pow']) < 1e-6),]
  res[[length(res) + 1]] <- best_modelfit_args_df
}

res <- do.call(rbind, res)


batchtools::batchMap(
  fun = birdflow_modelfit,
  args = res,
  reg = batchtools::makeRegistry(
    file.path('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/best_model_set/models', paste0(BirdFlowPipeline:::make_timestamp(), '_mf')),
    conf.file = system.file('batchtools.conf.R', 
                            package = 'BirdFlowPipeline')))

modelfit_resources <- list(walltime = 60,
                           ngpus = 1,
                           memory = params$gpu_ram + 1)
batchtools::submitJobs(dplyr::mutate(batchtools::findNotSubmitted(), chunk = 1L),
                       resources = modelfit_resources)


success <- batchtools::waitForJobs()

if (! isTRUE(success)) {
  message('Requeuing jobs that expired or had an error, attempt 1 of 2')
  batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), chunk = 1L),
                         resources = modelfit_resources)
  success <- batchtools::waitForJobs()
}
if (! isTRUE(success)) {
  message('Requeuing jobs that expired or had an error, attempt 2 of 2')
  batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), chunk = 1L),
                         resources = modelfit_resources)
  success <- batchtools::waitForJobs()
}
stopifnot(isTRUE(success))
invisible(success)










