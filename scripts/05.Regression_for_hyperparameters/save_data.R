library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/05.Regression_for_hyperparameters/')

load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.

source('../02.Summarize_validation_preliminary/load_data_functions.R')

## load data
res <- load_raw_validation_all_sp()
raw_combined <- res[['raw_combined']]
raw_combined_with_tracking <-  res[['raw_combined_with_tracking']]

res <- load_best_models_validation_all_sp(raw_combined, raw_combined_with_tracking)
all_res <- res[['all_res']]
all_res_with_tracking <- res[['all_res_with_tracking']]

##
all_res$base_method <- gsub('LOO_', '', all_res$method)
all_res$method_variation <- ifelse(grepl("^LOO_", all_res$method), "LOO", "species-specific")

##
all_res <- all_res[(all_res$base_method=='ST_and_LL_log') & (all_res$method=='ST_and_LL_log'),] # Remove th LOO

#
res <- list()
sp_count <- 0
for (sp in all_res$sp) {
  sp_count <- sp_count + 1
  print(sp)
  print(sp_count)
  
  model_name <- all_res[all_res$sp==sp,]$model
  model_path <- glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model_name}')
  model <- import_birdflow(model_path)
  
  ##
  nonbreeding_dist <- rowMeans(BirdFlowR::get_distr(model, which = BirdFlowR::lookup_season_timesteps(model, 'nonbreeding'), from_marginals = TRUE))
  xy <- BirdFlowR::i_to_xy(1:length(nonbreeding_dist), model)
  lonlat <- BirdFlowR::xy_to_latlon(xy$x, xy$y, model)
  lonlat$abu <- nonbreeding_dist
  lonlat <- lonlat[lonlat$abu>0,]
  nonbreeding_lon_c <- with(lonlat, weighted.mean(lon, abu))
  nonbreeding_lat_c <- with(lonlat, weighted.mean(lat, abu))
  nonbreeding_lat_max <- max(lonlat$lat)
  nonbreeding_lat_min <- min(lonlat$lat)
  nonbreeding_lon_max <- max(lonlat$lon)
  nonbreeding_lon_min <- min(lonlat$lon)
  nonbreeding_range_size <- dim(lonlat)[1]
  nonbreeding_abundance_variation <- sd(lonlat$abu)
  
  ##
  prebreeding_dist <- rowMeans(BirdFlowR::get_distr(model, which = BirdFlowR::lookup_season_timesteps(model, 'prebreeding'), from_marginals = TRUE))
  xy <- BirdFlowR::i_to_xy(1:length(prebreeding_dist), model)
  lonlat <- BirdFlowR::xy_to_latlon(xy$x, xy$y, model)
  lonlat$abu <- prebreeding_dist
  lonlat <- lonlat[lonlat$abu>0,]
  prebreeding_lon_c <- with(lonlat, weighted.mean(lon, abu))
  prebreeding_lat_c <- with(lonlat, weighted.mean(lat, abu))
  prebreeding_lat_max <- max(lonlat$lat)
  prebreeding_lat_min <- min(lonlat$lat)
  prebreeding_lon_max <- max(lonlat$lon)
  prebreeding_lon_min <- min(lonlat$lon)
  prebreeding_range_size <- dim(lonlat)[1]
  prebreeding_abundance_variation <- sd(lonlat$abu)
  
  ##
  breeding_dist <- rowMeans(BirdFlowR::get_distr(model, which = BirdFlowR::lookup_season_timesteps(model, 'breeding'), from_marginals = TRUE))
  xy <- BirdFlowR::i_to_xy(1:length(breeding_dist), model)
  lonlat <- BirdFlowR::xy_to_latlon(xy$x, xy$y, model)
  lonlat$abu <- prebreeding_dist
  lonlat <- lonlat[lonlat$abu>0,]
  breeding_lon_c <- with(lonlat, weighted.mean(lon, abu))
  breeding_lat_c <- with(lonlat, weighted.mean(lat, abu))
  breeding_lat_max <- max(lonlat$lat)
  breeding_lat_min <- min(lonlat$lat)
  breeding_lon_max <- max(lonlat$lon)
  breeding_lon_min <- min(lonlat$lon)
  breeding_range_size <- dim(lonlat)[1]
  breeding_abundance_variation <- sd(lonlat$abu)
  
  ##
  postbreeding_dist <- rowMeans(BirdFlowR::get_distr(model, which = BirdFlowR::lookup_season_timesteps(model, 'postbreeding'), from_marginals = TRUE))
  xy <- BirdFlowR::i_to_xy(1:length(postbreeding_dist), model)
  lonlat <- BirdFlowR::xy_to_latlon(xy$x, xy$y, model)
  lonlat$abu <- postbreeding_dist
  lonlat <- lonlat[lonlat$abu>0,]
  postbreeding_lon_c <- with(lonlat, weighted.mean(lon, abu))
  postbreeding_lat_c <- with(lonlat, weighted.mean(lat, abu))
  postbreeding_lat_max <- max(lonlat$lat)
  postbreeding_lat_min <- min(lonlat$lat)
  postbreeding_lon_max <- max(lonlat$lon)
  postbreeding_lon_min <- min(lonlat$lon)
  postbreeding_range_size <- dim(lonlat)[1]
  postbreeding_abundance_variation <- sd(lonlat$abu)
  
  res[[length(res) + 1]] <- data.frame(sp=sp, 
                                       nonbreeding_lon_c=nonbreeding_lon_c, nonbreeding_lat_c=nonbreeding_lat_c, 
                                       nonbreeding_lat_max=nonbreeding_lat_max, nonbreeding_lat_min=nonbreeding_lat_min, nonbreeding_lon_max=nonbreeding_lon_max, nonbreeding_lon_min=nonbreeding_lon_min,
                                       nonbreeding_range_size=nonbreeding_range_size, nonbreeding_abundance_variation=nonbreeding_abundance_variation,
                                       prebreeding_lon_c=prebreeding_lon_c, prebreeding_lat_c=prebreeding_lat_c, 
                                       prebreeding_lat_max=prebreeding_lat_max, prebreeding_lat_min=prebreeding_lat_min, prebreeding_lon_max=nonbreeding_lon_max, prebreeding_lon_min=prebreeding_lon_min,
                                       prebreeding_range_size=prebreeding_range_size, prebreeding_abundance_variation=prebreeding_abundance_variation,
                                       breeding_lon_c=breeding_lon_c, breeding_lat_c=breeding_lat_c, 
                                       breeding_lat_max=breeding_lat_max, breeding_lat_min=breeding_lat_min, breeding_lon_max=breeding_lon_max, breeding_lon_min=breeding_lon_min,
                                       breeding_range_size=breeding_range_size, breeding_abundance_variation=breeding_abundance_variation,
                                       postbreeding_lon_c=postbreeding_lon_c, postbreeding_lat_c=postbreeding_lat_c, 
                                       postbreeding_lat_max=postbreeding_lat_max, postbreeding_lat_min=postbreeding_lat_min, postbreeding_lon_max=postbreeding_lon_max, postbreeding_lon_min=postbreeding_lon_min,
                                       postbreeding_range_size=postbreeding_range_size, postbreeding_abundance_variation=postbreeding_abundance_variation
                                       )
  
  
  # if (length(res)>=5) {
  #   stop()
  # }
}

res <- as.data.frame(do.call(rbind, res))
all_res <- all_res |> merge(res, by.x='sp', by.y='sp', all.x=T)

# obs <- as.numeric(sub(".*_obs([0-9.]+)_ent.*", "\\1", all_res$model))
ent <- as.numeric(sub(".*_ent([0-9.]+)_dist.*", "\\1", all_res$model))
dist <- as.numeric(sub(".*_dist([0-9.]+)_pow.*", "\\1", all_res$model))
pow <- as.numeric(sub(".*_pow([0-9.]+)\\.hdf5", "\\1", all_res$model))
all_res <- cbind(all_res, data.frame(ent, dist, pow))

## Merge species traits data
trait_data <- read.csv('../../data/00.sp_info/All_combined_eco_function_traits.csv')
all_res <- all_res |>
  merge(ebirdst::ebirdst_runs[,c('species_code', 'common_name')], by.x = 'sp', by.y = 'species_code', all.x=T) |>
  merge(trait_data, by.x='common_name', by.y='Common_Name1_eBird', all.x=T)

##
write.csv(all_res, '../../data/03.All_validation_summary/Best_models_by_ST_and_LL_validation_summaries.csv')





