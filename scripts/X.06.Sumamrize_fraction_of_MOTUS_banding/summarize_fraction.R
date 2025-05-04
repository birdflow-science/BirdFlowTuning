
parent_path <- '/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric'
all_sp <- list.files(parent_path)

all_res <- list()
for (sp in all_sp){
  if (length(list.files(glue::glue('{parent_path}/{sp}'))) == 0) {
    next
  }
  
  train_file <- glue::glue('{parent_path}/{sp}/{sp}_150km_interval_based_eval_using_migration_transitions/each_transition_evaluation/train_each_transition_evaluation_all_combined_{sp}_2022_150km_obs1.0_ent0.010526_dist0.042105_pow0.2.hdf5.rds')
  test_file <- glue::glue('{parent_path}/{sp}/{sp}_150km_interval_based_eval_using_migration_transitions/each_transition_evaluation/test_each_transition_evaluation_all_combined_{sp}_2022_150km_obs1.0_ent0.010526_dist0.042105_pow0.2.hdf5.rds')
  if (!(file.exists(train_file) && file.exists(test_file))) {
    next
  }
  
  train <- readRDS(train_file)
  test <- readRDS(test_file)
  
  all_res[[length(all_res) + 1]] <- list(
    sp = sp,
    train_n_banding = sum(train$route_type=='banding'),
    train_n_tracking = sum(train$route_type=='tracking'),
    train_n_motus = sum(train$route_type=='motus'),
    test_n_banding = sum(test$route_type=='banding'),
    test_n_tracking = sum(test$route_type=='tracking'),
    test_n_motus = sum(test$route_type=='motus')
  )
}

all_res <- do.call(rbind.data.frame, all_res)

trait_data <- read.csv('../../data/00.sp_info/All_combined_eco_function_traits.csv')
compared_df <- all_res |>
  merge(ebirdst::ebirdst_runs[,c('species_code', 'common_name')], by.x = 'sp', by.y = 'species_code', all.x=T)
write.csv(compared_df, '../../data/04.Data_summary/transition_data_summary.csv', quote=FALSE, row.names=F)

