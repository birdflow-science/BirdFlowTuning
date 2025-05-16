library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)
library(ebirdst)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/02.Summarize_validation_preliminary/')
source('load_data_functions.R')

parent_path <- "/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric"

all_combined <- list()
for (n_transitions in c(20, 50, 100, 200, 500, 800, 1000, 2000, 3000)) {
  paths <- Sys.glob(glue::glue("{parent_path}/*/*_150km_interval_based_eval_using_migration_transitions_n_transitions{n_transitions}"))
  
  ## Load data round 1
  res <- load_raw_validation_all_sp(paths, search_cv=T, average_cv=T) # This will load the training results only, no validation.
  raw_combined <- res[['raw_combined']]
  raw_combined_with_tracking <- res[['raw_combined_with_tracking']]
  
  if (!inherits(raw_combined, "data.frame")) {
    next
  }
  
  ## Load data round 2
  res <- load_best_models_validation_all_sp(raw_combined, raw_combined_with_tracking, paths, include_taxomany_LOO=TRUE)
  all_res <- res[['all_res']] # this will be saved later, after merging with other information
  all_res_with_tracking <- res[['all_res_with_tracking']]
  all_combined[[length(all_combined) + 1]] <- all_res
}

all_combined <- do.call(rbind, all_combined)
all_combined <- all_combined[all_combined$method=='ST098_and_LL',]

library(ggplot2)
ggplot(data=all_combined, aes(x=training_n_intervals, y=weighted_mean_ll_improvement, color=sp)) +
  geom_point() +
  geom_line()


# 
# ggplot(data=all_raw_combined, aes(x=n_transitions, y=weighted_mean_win_distance_fraction, color=sp)) +
#   geom_point() +
#   geom_line()
# 
# ggplot(data=all_raw_combined, aes(x=n_transitions, y=mean_win_distance_fraction, color=sp)) +
#   geom_point() +
#   geom_line()
# 
# 
