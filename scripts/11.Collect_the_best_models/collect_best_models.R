library(ggplot2)
setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/11.Collect_the_best_models/')
source('../plotting_params/plotting_params.R')
results <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
results <- results |> dplyr::group_by(.data[['sp']], .data[['method']]) |> dplyr::slice(1) |> dplyr::ungroup()
results <- results[results$method=='ST098_and_LL',]

results$path <- mapply(function(sp, model) {
  glue::glue("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model}")
}, results$sp, results$model)
write.csv(results, '/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/best_models_ST098_and_LL.csv')


for (row_idx in 1:nrow(results)) {
  print(row_idx)
  sp_code <- results[row_idx, ]$sp
  source_file <- glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp_code}/{sp_code}_150km/{results[row_idx, ]$model}')
  target_folder <- glue::glue('/home/yc85_illinois_edu/BirdFlow_Validation_Project/models/{sp_code}')
  if (!dir.exists(target_folder)) {
    dir.create(target_folder, recursive = TRUE)
  }
  
  file.copy(source_file, glue::glue("{target_folder}/{sp_code}_tuned.hdf5"))
}
