library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/09.Comparing_training_with_and_without_log/')

devtools::install_local("/home/yc85_illinois_edu/BirdFlowR", force = T, dependencies = FALSE) # if the BirdFlowR is updated, we need to reinstall it, so that i can be used in BirdFlowPipeline!
devtools::install_local("/home/yc85_illinois_edu/BirdFlowPipeline", force = T, dependencies = FALSE)

load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.

## load data 1
source('../02.Summarize_validation_preliminary/load_data_functions_log_obs.R')
res <- load_raw_validation_all_sp()
raw_combined_log_obs <- res[['raw_combined']]
raw_combined_with_tracking_log_obs <-  res[['raw_combined_with_tracking']]
plot(raw_combined_log_obs$end_traverse_cor, raw_combined_log_obs$weighted_mean_ll_improvement)
plot(raw_combined_log_obs$weighted_mean_ll_improvement, raw_combined_log_obs$mean_win_distance_fraction)

res <- load_best_models_validation_all_sp(raw_combined_log_obs, raw_combined_with_tracking_log_obs)
all_res_log_obs <- res[['all_res']]
all_res_with_tracking_log_obs <- res[['all_res_with_tracking']]

## load data 2
source('../02.Summarize_validation_preliminary/load_data_functions.R')
res <- load_raw_validation_all_sp()
raw_combined <- res[['raw_combined']]
raw_combined_with_tracking <-  res[['raw_combined_with_tracking']]

res <- load_best_models_validation_all_sp(raw_combined, raw_combined_with_tracking)
all_res <- res[['all_res']]
all_res_with_tracking <- res[['all_res_with_tracking']]

### Compare
sp <- 'treswa'
tmp1 <- raw_combined[raw_combined$sp==sp,] |> dplyr::arrange(.data[['model']])
tmp2 <- raw_combined_log_obs[raw_combined_log_obs$sp==sp,] |> dplyr::arrange(.data[['model']])

hist(tmp1$end_traverse_cor - tmp2$end_traverse_cor, breaks=100)
hist(tmp1$end_traverse_cor_log - tmp2$end_traverse_cor_log, breaks=100)

hist(tmp1$weighted_mean_ll_improvement - tmp2$weighted_mean_ll_improvement, breaks=100) # So no log is actually better!
hist(tmp1$weighted_mean_win_distance_fraction - tmp2$weighted_mean_win_distance_fraction, breaks=100)
hist(tmp1$weighted_energy_improvement - tmp2$weighted_energy_improvement, breaks=100) # So no log is actually better!

all_res[(all_res$sp==sp) & (all_res$method=='ST_and_LL_log'),]$weighted_mean_ll_improvement
all_res_log_obs[(all_res_log_obs$sp==sp) & (all_res_log_obs$method=='ST_and_LL_log'),]$weighted_mean_ll_improvement

all_res[(all_res$sp==sp) & (all_res$method=='ST_and_LL_log'),]$weighted_energy_improvement
all_res_log_obs[(all_res_log_obs$sp==sp) & (all_res_log_obs$method=='ST_and_LL_log'),]$weighted_energy_improvement

all_res[(all_res$sp==sp) & (all_res$method=='ST_and_LL'),]$weighted_energy_improvement
all_res_log_obs[(all_res_log_obs$sp==sp) & (all_res_log_obs$method=='ST_and_LL'),]$weighted_energy_improvement

plot(tmp1$obs_prop, tmp1$end_traverse_cor)
plot(tmp2$obs_prop, tmp2$end_traverse_cor)

plot(tmp1$obs_prop, tmp1$end_traverse_cor_log)
plot(tmp2$obs_prop, tmp2$end_traverse_cor_log)

### So we should not use log, no improvement



