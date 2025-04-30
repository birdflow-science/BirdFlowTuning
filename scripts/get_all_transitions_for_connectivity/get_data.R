library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/get_all_transitions_for_connectivity')

load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.

## process one species
args <- commandArgs(trailingOnly = TRUE)
species <- args[1]

get_data <- function(species) {
  sp_output_path <- paste0('/project/pi_drsheldon_umass_edu/birdflow/data_for_connectivity_calculation/', species)
  if (!dir.exists(sp_output_path)){dir.create(sp_output_path, recursive = TRUE)}
  
  params <- set_pipeline_params(species = species, 
                                gpu_ram = 10,
                                hdf_path = sp_output_path,
                                base_output_path = sp_output_path,
                                model_selection = 'distance_metric',
                                suffix='transition_data',
                                skip_quality_checks=TRUE,
                                min_season_quality = 1)
  params <- preprocess_species_wrapper(params)
  
  if (file.exists(file.path(params$output_path, 'all_ground_truth_transitions_df_filtered_transitions_between_breeding_and_nonbreeding.rds'))) {
    stop('All finished!')
  }
  
  saveRDS(params, file.path(params$output_path, 'params.rds'))
  
  # # Batch fit models
  # batch_modelfit_wrapper(params)

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
  
  # Routes to BirdFlowRoutes to BirdFlowIntervals
  # Load and save track info
  # Here should combine banding and motus data and convert to BirdFlowIntervals class
  banding_df <- load_banding_transitions_df(file.path(BirdFlowPipeline:::the$banding_rds_path, paste0(params$species, '.rds')))
  motus_df <- load_motus_transitions_df(file.path(BirdFlowPipeline:::the$motus_rds_path, paste0(params$species, '.rds')))
  track_birdflowroutes_obj <- get_real_track(bf, params, filter=FALSE) # Real track. Not filtered by season. All year round.
  combined_data <- rbind(banding_df, motus_df, track_birdflowroutes_obj$data[,c('route_id','date','lon','lat','route_type')])
  combined_data <- na.omit(combined_data)
  saveRDS(combined_data, file.path(params$output_path, 'all_ground_truth_traj_df.rds'))
  
  # Dataframe to Routes
  source <- ''
  if (!is.null(banding_df)){
    if (source==''){
      source <- 'Banding'
    } else {
      source <- paste0(source, ' & ', 'Banding')
    }
  } else if (!is.null(motus_df)){
    if (source==''){
      source <- 'MOTUS'
    } else {
      source <- paste0(source, ' & ', 'MOTUS')
    }
  } else if (!is.null(track_birdflowroutes_obj)){
    if (source==''){
      source <- 'Tracking'
    } else {
      source <- paste0(source, ' & ', 'Tracking')
    }
  }
  
  if (source==''){
    source <- 'No Data'
  } 
  
  ## All interval samples
  routes_obj <- BirdFlowR::Routes(combined_data, species=bf$species, source=source)
  if (nrow(routes_obj$data)==0){
    stop("No Transition data available")
  }
  birdflow_routes_obj <- routes_obj |> BirdFlowR::as_BirdFlowRoutes(bf=bf)
  interval_obj <- birdflow_routes_obj |>
    BirdFlowR::as_BirdFlowIntervals(max_n=10000,
                                    min_day_interval=1,
                                    max_day_interval=270,
                                    min_km_interval=0,
                                    max_km_interval=8000)

  saveRDS(interval_obj$data, file.path(params$output_path, 'all_ground_truth_transitions_df_no_filter.rds'))
  
  # Filter intervals to ask at that the start is in breeding season and end in non-breeding, or vice versa
  target_timesteps1 <- BirdFlowR::lookup_season_timesteps(bf, season='breeding')
  target_timesteps2 <- BirdFlowR::lookup_season_timesteps(bf, season='nonbreeding')

  interval_obj$data <- interval_obj$data[
    ((interval_obj$data$timestep1 %in% target_timesteps1) & (interval_obj$data$timestep2 %in% target_timesteps2)) |
      ((interval_obj$data$timestep1 %in% target_timesteps2) & (interval_obj$data$timestep2 %in% target_timesteps1))
    ,]
  saveRDS(interval_obj$data, file.path(params$output_path, 'all_ground_truth_transitions_df_filtered_transitions_between_breeding_and_nonbreeding.rds'))
}

## Run
get_data(species)



