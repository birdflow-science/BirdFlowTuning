library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/02.Summarize_validation_preliminary/')

# load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
# load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.

source('load_data_functions.R')

# paths <-c()
# for (sp in c('acafly', 'ameavo')) {
#   path <- glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_interval_based_eval_using_migration_transitions')
#   paths <- c(paths, path)
# }

### Step 1: load validation data
## load data of all models in grid search
res <- load_raw_validation_all_sp(search_cv=F)
raw_combined <- res[['raw_combined']]
raw_combined_with_tracking <-  res[['raw_combined_with_tracking']]

## Summarize the best models based on model selection method
res <- load_best_models_validation_all_sp(raw_combined, raw_combined_with_tracking, 
                                          search_cv=F, include_taxomany_LOO=TRUE)
all_res <- res[['all_res']] # this will be saved later, after merging with other information
all_res_with_tracking <- res[['all_res_with_tracking']]

### Step 2: tears apart hyperparameters
## isolate the parameters
library(stringr)
pattern <- paste0("_ent([0-9\\.]+(?:[eE][+-]?[0-9]+)?)",
                  "_dist([0-9\\.]+(?:[eE][+-]?[0-9]+)?)",
                  "_pow([0-9\\.]+(?:[eE][+-]?[0-9]+)?)",
                  "\\.hdf5$")
m <- str_match(all_res$model, pattern)
all_res$ent  <- as.numeric(m[,2])
all_res$dist <- as.numeric(m[,3])
all_res$pow  <- as.numeric(m[,4])

### Step 3: summarize data used for tuning the model
data_summary <- list()
all_sp <- all_res$sp |> unique()
parent_path <- parent_path
for (sp in all_sp){
  if (length(list.files(glue::glue('{parent_path}/{sp}'))) == 0) {
    next
  }
  
  train_file <- glue::glue('{parent_path}/{sp}/{sp}_150km_interval_based_eval_using_migration_transitions/each_transition_evaluation/train_each_transition_evaluation_all_combined_{sp}_2023_150km_obs1.0_ent0.010526_dist0.042105_pow0.2.hdf5.rds')
  test_file <- glue::glue('{parent_path}/{sp}/{sp}_150km_interval_based_eval_using_migration_transitions/each_transition_evaluation/test_each_transition_evaluation_all_combined_{sp}_2023_150km_obs1.0_ent0.010526_dist0.042105_pow0.2.hdf5.rds')
  if (!(file.exists(train_file) && file.exists(test_file))) {
    next
  }
  
  train <- readRDS(train_file)
  test <- readRDS(test_file)
  
  data_summary[[length(data_summary) + 1]] <- list(
    sp = sp,
    train_n_banding = sum(train$route_type=='banding'),
    train_n_tracking = sum(train$route_type=='tracking'),
    train_n_motus = sum(train$route_type=='motus'),
    test_n_banding = sum(test$route_type=='banding'),
    test_n_tracking = sum(test$route_type=='tracking'),
    test_n_motus = sum(test$route_type=='motus')
  )
}

data_summary <- do.call(rbind.data.frame, data_summary)
merged_df <- all_res |> merge(
  data_summary, by.x = 'sp', by.y = 'sp', all.x=T
)
raw_combined <- raw_combined |> merge(
  data_summary, by.x = 'sp', by.y = 'sp', all.x=T
)
raw_combined_with_tracking <- raw_combined_with_tracking |> merge(
  data_summary, by.x = 'sp', by.y = 'sp', all.x=T
)
all_res_with_tracking <- all_res_with_tracking |> merge(
  data_summary, by.x = 'sp', by.y = 'sp', all.x=T
)

### Step 4: get S&T model quality
merged_df <- merged_df |> merge(
  ebirdst::ebirdst_runs[,c('species_code','common_name','nonbreeding_quality','prebreeding_migration_quality',
                         'breeding_quality','postbreeding_migration_quality')], 
  by.x = 'sp', by.y = 'species_code', all.x=T
  
)

### Step 5: merge with eco and taxonomy data
trait_data <- read.csv('../../data/00.sp_info/All_combined_eco_function_traits.csv')
merged_df <- merged_df |>
  merge(trait_data, by.x = 'common_name', by.y = 'Common_Name1_eBird', all.x=T)

### Step 6: Filter out species with n_training_transition < 10
merged_df <- merged_df[
  (merged_df$train_n_banding + merged_df$train_n_tracking + merged_df$train_n_motus > 10) &
    (merged_df$test_n_banding + merged_df$test_n_tracking + merged_df$test_n_motus > 10)
  ,]
raw_combined <- raw_combined[raw_combined$sp %in% merged_df$sp|>unique(),]
raw_combined_with_tracking <- raw_combined_with_tracking[raw_combined_with_tracking$sp %in% merged_df$sp|>unique(),]
all_res_with_tracking <- all_res_with_tracking[all_res_with_tracking$sp %in% merged_df$sp|>unique(),]

## Finally 1: Save the data
write.csv(merged_df, '../../data/03.All_validation_summary/validation_final_summary.csv', quote=TRUE, row.names=F) # Main dataset
write.csv(raw_combined, '../../data/03.All_validation_summary/validation_all_models_gridsearch_summary.csv', quote=TRUE, row.names=F)
write.csv(raw_combined_with_tracking, '../../data/03.All_validation_summary/validation_all_models_gridsearch_tracking_sp_only_summary.csv', quote=TRUE, row.names=F)
write.csv(all_res_with_tracking, '../../data/03.All_validation_summary/validation_compare_methods_tracking_sp_only_summary.csv', quote=TRUE, row.names=F)

## Finally 2: Save the data
merged_df_filtered <- merged_df[merged_df$method %in% c('ST098_and_LL', 'LOO_ST098_and_LL', 'LOO_FAMILY_ST098_and_LL', 'LOO_ORDER_ST098_and_LL'),]
write.csv(merged_df_filtered, '../../data/03.All_validation_summary/validation_final_summary_filtered.csv', quote=TRUE, row.names=F)



