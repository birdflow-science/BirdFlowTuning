##
library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.


species = sp = 'cangoo'
training_n_transitions=NULL
training_CV=1
use_cached_data=T
cached_path = glue::glue("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{species}")

gpu_ram = 10
hdf_path = glue::glue("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{species}")
base_output_path = glue::glue("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{species}")
model_selection = 'distance_metric'
suffix='interval_based_eval_using_migration_transitions'
params <- set_pipeline_params(species = species, gpu_ram = 10,
                              hdf_path = glue::glue("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{species}"),
                              base_output_path = glue::glue("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{species}"),
                              model_selection = 'distance_metric',
                              suffix='interval_based_eval_using_migration_transitions')

params <- preprocess_species_wrapper(params)


# Batch fit models
batch_modelfit_wrapper(params)

# Exit if fitting only
if (isTRUE(params$fit_only)){
  return(NULL)
}

# Get bf object (for converting to BirdFlowIntervals)
pp_dir <- tempdir()
bf <- BirdFlowR::preprocess_species(
  species = params$species,
  out_dir = pp_dir,
  gpu_ram = params$gpu_ram,
  res = params$res,
  season = dplyr::if_else(params$truncate_season, params$season, 'all'),
  clip = params$clip,
  crs = params$crs,
  skip_quality_checks = params$skip_quality_checks, 
  trim_quantile = params$trim_quantile
)
bf$metadata <- params$metadata


# Load consensus data or load from multtsouce on the fly
all_ground_truth_transitions_df_path1 <- file.path(params$output_path, 'all_ground_truth_transitions_df.rds')
consensus_routes_path <- paste0(the$combined_data_path_routes, '/', species, '.hdf5')
consensus_interval_path <- paste0(the$combined_data_path_birdflowintervals, '/', species, '.hdf5')

if (use_cached_data &&
    file.exists(consensus_routes_path) &&
    file.exists(consensus_interval_path)) {
  print('Loading cached training/validation data...')
  
  ## combined_routes_data
  combined_routes_obj <- BirdFlowR::read_routes(consensus_routes_path)
  combined_routes_data <- combined_routes_obj$data
  saveRDS(combined_routes_data, all_ground_truth_transitions_df_path1) # normal dir
  
  ## Intervals
  interval_obj <- BirdFlowR::read_intervals(consensus_interval_path)
  target_timesteps <- c(BirdFlowR::lookup_season_timesteps(bf, season='prebreeding'),  # Filter intervals to ask at least one leg in the migration season
                        BirdFlowR::lookup_season_timesteps(bf, season='postbreeding'))
  interval_obj$data <- interval_obj$data[(interval_obj$data$timestep1 %in% target_timesteps) | (interval_obj$data$timestep2 %in% target_timesteps),]
  interval_one_week_obj <- interval_obj
  interval_one_week_obj$data <-interval_one_week_obj$data[interval_one_week_obj$data$timestep2 - interval_one_week_obj$data$timestep1 == 1,]
  
  ## downsample
  interval_obj$data <- interval_obj$data |> dplyr::arrange(-(date2 - date1)) |> dplyr::slice_tail(n = 10000) # maximum 10000
  
} else {
  if (use_cached_data) {
    stop('Althought use_cached_data, no cached data available. Maybe be data is not abundant for this species. 
           Or maybe you need a new version of the consensus data.')
  }
  res <- get_ground_truth_routes_intervals_and_one_week_intervals(params, bf)
  combined_routes_data <- res[['combined_routes_data']]
  interval_obj <- res[['interval_obj']]
  interval_one_week_obj <- res[['interval_one_week_obj']]
  
  saveRDS(combined_routes_data, all_ground_truth_transitions_df_path1) # normal dir
}

if (nrow(interval_obj$data) <= 20) {
  stop('Not enough transition data!')
}

## For MC
routes_data_for_mc <- combined_routes_data |> 
  BirdFlowR::Routes(species=bf$species,
                    source=bf$source) |> 
  BirdFlowR::as_BirdFlowRoutes(bf=bf)

#
params$transition_type <- 'all_combined'

# Train-test split
set.seed(42)
train_data <- interval_obj$data |> dplyr::sample_frac(0.7, replace = FALSE)
test_data <- dplyr::setdiff(interval_obj$data, train_data)
set.seed(42)
if (is.null(training_n_transitions)) {
  ## Nothing happens
} else {
  if (nrow(train_data) < training_n_transitions) {
    stop(glue::glue('Cannot sample {training_n_transitions} transitions -- not enough data.'))
  } else {
    train_data <- train_data|> dplyr::sample_n(training_n_transitions, replace = FALSE) ## Subsample, for learning curve analysis
  }
}

train_data <- BirdFlowR::BirdFlowIntervals(data=train_data,
                                           species=interval_obj$species,
                                           metadata=interval_obj$metadata,
                                           geom=interval_obj$geom,
                                           dates=interval_obj$dates,
                                           source=interval_obj$source)
test_data <- BirdFlowR::BirdFlowIntervals(data=test_data,
                                          species=interval_obj$species,
                                          metadata=interval_obj$metadata,
                                          geom=interval_obj$geom,
                                          dates=interval_obj$dates,
                                          source=interval_obj$source)

set.seed(42)
train_data_one_week <- interval_one_week_obj$data |> dplyr::sample_frac(0.7, replace = FALSE)
test_data_one_week <- dplyr::setdiff(interval_one_week_obj$data, train_data_one_week)
train_data_one_week <- BirdFlowR::BirdFlowIntervals(data=train_data_one_week,
                                                    species=interval_one_week_obj$species,
                                                    metadata=interval_one_week_obj$metadata,
                                                    geom=interval_one_week_obj$geom,
                                                    dates=interval_one_week_obj$dates,
                                                    source=interval_one_week_obj$source)
test_data_one_week <- BirdFlowR::BirdFlowIntervals(data=test_data_one_week,
                                                   species=interval_one_week_obj$species,
                                                   metadata=interval_one_week_obj$metadata,
                                                   geom=interval_one_week_obj$geom,
                                                   dates=interval_one_week_obj$dates,
                                                   source=interval_one_week_obj$source)

saveRDS(train_data, file.path(params$output_path, glue::glue('train_data_all_combined.rds')))
saveRDS(test_data, file.path(params$output_path, glue::glue('test_data_all_combined.rds')))
saveRDS(train_data_one_week, file.path(params$output_path, glue::glue('train_data_one_week_all_combined.rds')))
saveRDS(test_data_one_week, file.path(params$output_path, glue::glue('test_data_one_week_all_combined.rds')))

# Batch model evaluation
if (!dir.exists(file.path(params$output_path, 'each_transition_evaluation'))){
  dir.create(file.path(params$output_path, 'each_transition_evaluation'))
}

if (!is.numeric(training_CV)) {
  stop('training_CV should be numeric!')
}

idx <- sample.int(nrow(train_data$data))
if (training_CV==1) {
  parts <- list(`1`=idx)
} else {
  parts <- split(
    idx,
    cut(seq_along(idx), breaks = training_CV, labels = FALSE)
  )
}


## Run CV
cv_iter=1
the_idx <- parts[[as.character(cv_iter)]]
this_training_data <- train_data
this_training_data$data <- this_training_data$data[the_idx, ]

params$mode <- 'train'

## evaluation  
birdflow_intervals = this_training_data
birdflow_intervals_one_week = train_data_one_week
files <- list.files(path = params$hdf_dir,
                    pattern = paste0('^', params$species, '.*', params$res, 'km_.*\\.hdf5$'),
                    full.names = TRUE)
path = files[1]
bf <- BirdFlowR::import_birdflow(path)
modelname = basename(path)

# evaluate_model(bf, modelname = basename(path), params=params, birdflow_intervals=birdflow_intervals, birdflow_intervals_one_week=birdflow_intervals_one_week, routes_data_for_mc=routes_data_for_mc)

connectivity_res <- calc_connectivity_metric(routes_data_for_mc, bf = bf, n_boot=5, max_n_each_season = 2000)
result <- BirdFlowR::calc_interval_metrics(birdflow_intervals, bf = bf)
interval_based_metrics <- result[[1]]
metric_for_each_transition <- result[[2]]
metric_for_each_transition$route_type <- birdflow_intervals$data$route_type
motus_fraction <- mean(c(metric_for_each_transition$route_type)=='motus')
banding_fraction <- mean(c(metric_for_each_transition$route_type)=='banding')
tracking_fraction <- mean(c(metric_for_each_transition$route_type)=='tracking')
saveRDS(metric_for_each_transition, file.path(params$output_path, 'each_transition_evaluation' ,glue::glue('{params$mode}_each_transition_evaluation_{params$transition_type}_{modelname}.rds')))





