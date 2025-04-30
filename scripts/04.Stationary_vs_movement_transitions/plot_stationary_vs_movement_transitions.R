library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/04.Stationary_vs_movement_transitions/')

# devtools::install_local("/home/yc85_illinois_edu/BirdFlowR", force = T, dependencies = FALSE) # if the BirdFlowR is updated, we need to reinstall it, so that i can be used in BirdFlowPipeline!
# devtools::install_local("/home/yc85_illinois_edu/BirdFlowPipeline", force = T, dependencies = FALSE)

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
# paths <- Sys.glob("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/*/*_150km_interval_based_eval")

sp <- 'amewoo'
model <- all_res[(all_res$sp==sp) & (all_res$method=='LL'),]$model[1]
each_transition_df <- readRDS(glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_interval_based_eval/each_transition_evaluation/test_each_transition_evaluation_all_combined_{model}.rds'))

plot(each_transition_df$elapsed_days, each_transition_df$energy_improvement)
plot(each_transition_df$elapsed_days, each_transition_df$ll - each_transition_df$null_ll)
abline(h = 0, col = "red", lty = 2)
plot(each_transition_df$elapsed_days, each_transition_df$win_distance_fraction)

plot(each_transition_df$elapsed_km, each_transition_df$energy_improvement)
plot(each_transition_df$elapsed_km, each_transition_df$ll - each_transition_df$null_ll)
abline(h = 0, col = "red", lty = 2)
plot(each_transition_df$elapsed_km, each_transition_df$win_distance_fraction)


