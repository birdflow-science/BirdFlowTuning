library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)
library(ggplot2)

date_ <- '2025-04-02'
species_list <- c('amewoo', 'buwtea', 'swahaw', 'brwhaw', 'woothr', 'lobcur') #, 'lobcur',  'buwtea', 
plot_list_cor90 <- list()
plot_list_model_selection <- list()
plot_list_pit_d_by_model_selection <- list()
plot_list_str_d_and_nso_d_by_model_selection <- list()
plot_list_str_d_and_speed_d_by_model_selection <- list()
all_res <- list()
for (sp in species_list){

  eval_metrics_train_distance_metric_path <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_{date_}/eval_metrics_train_distance_metric_all_combined.rds')
  eval_metrics_train_multi_objective_path <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_{date_}/eval_metrics_train_multi_objective_all_combined.rds')
  eval_metrics_test_distance_metric_path <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_{date_}/eval_metrics_test_distance_metric_all_combined.rds')
  
  train_data <-readRDS(eval_metrics_train_distance_metric_path)
  train_data$weighted_mean_ll_improvement <- train_data$weighted_mean_ll - train_data$weighted_mean_null_ll
  train_data_multi_objective <-readRDS(eval_metrics_train_multi_objective_path)
  train_data_multi_objective$weighted_mean_ll_improvement <- train_data_multi_objective$weighted_mean_ll - train_data_multi_objective$weighted_mean_null_ll
  train_data <- train_data_multi_objective |> merge(train_data_multi_objective[,c('model', 'overall_des')] |> dplyr::rename(multi_objective=overall_des)
                                         , by = 'model')
  test_data <-readRDS(eval_metrics_test_distance_metric_path)
  test_data$weighted_mean_ll_improvement <- test_data$weighted_mean_ll - test_data$weighted_mean_null_ll
  test_data <- test_data |> merge(train_data_multi_objective[,c('model', 'overall_des', 'pit_d', 'straightness_diff', 'n_stopovers_diff', 'speed_diff')] |> dplyr::rename(multi_objective=overall_des)
                                  , by = 'model')
  
  models <- rep(NA, nrow(test_data))
  for (row_idx in 1:nrow(test_data)){
    the_model_name <- test_data$model[row_idx]
    
    this_models_feature <- ''
    
    # LL + distance
    if (the_model_name==train_data[order(-train_data$weighted_mean_effective_win_distance),]$model[1]) {
      if (this_models_feature==''){
        this_models_feature <- 'best_by_weighted_mean_effective_win_distance'
      } else {
        this_models_feature <- paste0(this_models_feature, ' &\n', 'best_by_weighted_mean_effective_win_distance')
      }
    }
    if (the_model_name==train_data[train_data$end_traverse_cor>0.9, ][order(-train_data[train_data$end_traverse_cor>0.9, ]$weighted_mean_effective_win_distance),]$model[1]){
      if (this_models_feature==''){
        this_models_feature <- 'best_by_ST09_threshold_weighted_mean_effective_win_distance'
      } else {
        this_models_feature <- paste0(this_models_feature, ' &\n', 'best_by_ST09_threshold_weighted_mean_effective_win_distance')
      }
    }
    
    train_data <- train_data |> dplyr::mutate(score=scale(weighted_mean_win_distance) + scale(weighted_mean_ll_improvement)) |> dplyr::arrange(-score)
    if (the_model_name==train_data[train_data$end_traverse_cor>0.9, ]$model[1]){
      if (this_models_feature==''){
        this_models_feature <- 'best_by_ST09_threshold_LL+Distance'
      } else {
        this_models_feature <- paste0(this_models_feature, ' &\n', 'best_by_ST09_threshold_LL+Distance')
      }
    }
    
    if (the_model_name==train_data[order(-train_data$multi_objective),]$model[1]) {
      if (this_models_feature==''){
        this_models_feature <- 'best_by_multi_objective'
      } else {
        this_models_feature <- paste0(this_models_feature, ' &\n', 'best_by_multi_objective')
      }
    }
    
    if (the_model_name==train_data[order(-train_data$weighted_mean_ll_improvement),]$model[1]) {
      if (this_models_feature==''){
        this_models_feature <- 'best_by_LL'
      } else {
        this_models_feature <- paste0(this_models_feature, ' &\n', 'best_by_LL')
      }
    }
    
    if (the_model_name==train_data[order(-train_data$weighted_mean_win_distance),]$model[1]) {
      if (this_models_feature==''){
        this_models_feature <- 'best_by_Distance'
      } else {
        this_models_feature <- paste0(this_models_feature, ' &\n', 'best_by_Distance')
      }
    }
    
    if (the_model_name==train_data[order(-train_data$weighted_energy_improvement),]$model[1]) {
      if (this_models_feature==''){
        this_models_feature <- 'best_by_weighted_energy_improvement'
      } else {
        this_models_feature <- paste0(this_models_feature, ' &\n', 'best_by_weighted_energy_improvement')
      }
    }
    
    if (the_model_name==train_data[order(-train_data$weighted_energy_improvement_days_integral),]$model[1]) {
      if (this_models_feature==''){
        this_models_feature <- 'best_by_weighted_energy_improvement_days_integral'
      } else {
        this_models_feature <- paste0(this_models_feature, ' &\n', 'best_by_weighted_energy_improvement_days_integral')
      }
    }
    
    if (the_model_name==train_data[order(-train_data$weighted_energy_improvement_days_integral),]$model[1]) {
      if (this_models_feature==''){
        this_models_feature <- 'best_by_weighted_energy_improvement_kmss_integral'
      } else {
        this_models_feature <- paste0(this_models_feature, ' &\n', 'best_by_weighted_energy_improvement_kms_integral')
      }
    }
    
    if (this_models_feature==''){
      models[row_idx] <- 'nothing'
    } else {
      models[row_idx] <- this_models_feature
    }
    
  }
  
  test_data$models <- models
  
  library(RColorBrewer)
  non_nothing_levels <- unique(test_data$models[test_data$models != "nothing"])
  brewer_colors <- brewer.pal(n = length(non_nothing_levels), name = "Set1")
  names(brewer_colors) <- non_nothing_levels
  all_colors <- c("nothing" = "gray", brewer_colors)
  p <- ggplot(test_data, aes(x = weighted_mean_win_distance, y = weighted_mean_ll_improvement)) +
    geom_point(aes(color = ifelse(models == "nothing", "nothing", models),
                   alpha = ifelse(models == "nothing", 0.3, 0.8)), size = 3) +
    labs(
      title = sp,
      x = "Win Distance",
      y = "LL Improvement",
      color = 'Models'
    ) +
    scale_color_manual(values = all_colors) +
    scale_alpha_identity() +
    theme_minimal()
  plot_list_model_selection[[length(plot_list_model_selection)+1]] <- p
  
  test_data$end_traverse_cor_over_90 <- ifelse(test_data$end_traverse_cor>0.90, 'yes', 'no')
  p <- ggplot(test_data, aes(x = weighted_mean_win_distance, y = weighted_mean_ll_improvement, color=end_traverse_cor_over_90)) +
    geom_point(size = 3, alpha=0.7) +
    labs(
      title = sp,
      x = "Win Distance",
      y = "LL Improvement",
      color = 'end_traverse_cor_over_90'
    ) +
    scale_color_brewer(palette='Set1') +
    theme_minimal()
  plot_list_cor90[[length(plot_list_cor90)+1]] <- p
  
  p <- ggplot(test_data, aes(x = pit_d, y = weighted_mean_ll_improvement)) +
    geom_point(size = 3, aes(color = ifelse(models == "nothing", "nothing", models),
                                        alpha = ifelse(models == "nothing", 0.3, 0.8))) +
    labs(
      title = sp,
      x = "pit_d",
      y = "LL Improvement",
      color = 'Models'
    ) +
    scale_color_manual(values = all_colors) +
    scale_alpha_identity() +
    theme_minimal()
  plot_list_pit_d_by_model_selection[[length(plot_list_pit_d_by_model_selection)+1]] <- p

  p <- ggplot(test_data, aes(x = straightness_diff, y = n_stopovers_diff)) +
    geom_point(size = 3, aes(color = ifelse(models == "nothing", "nothing", models),
                             alpha = ifelse(models == "nothing", 0.3, 0.8))) +
    labs(
      title = sp,
      x = "straigtness differences",
      y = "n_stopovers differences",
      color = 'Models'
    ) +
    scale_color_manual(values = all_colors) +
    scale_alpha_identity() +
    theme_minimal()
  plot_list_str_d_and_nso_d_by_model_selection[[length(plot_list_str_d_and_nso_d_by_model_selection)+1]] <- p
  
  p <- ggplot(test_data, aes(x = straightness_diff, y = speed_diff)) +
    geom_point(size = 3, aes(color = ifelse(models == "nothing", "nothing", models),
                             alpha = ifelse(models == "nothing", 0.3, 0.8))) +
    labs(
      title = sp,
      x = "straigtness differences",
      y = "speed differences",
      color = 'Models'
    ) +
    scale_color_manual(values = all_colors) +
    scale_alpha_identity() +
    theme_minimal()
  plot_list_str_d_and_speed_d_by_model_selection[[length(plot_list_str_d_and_speed_d_by_model_selection)+1]] <- p
  
  test_data$sp <- sp
  all_res[[length(all_res) + 1]] <- test_data
}

n_row <- ifelse(length(plot_list_model_selection) %% 3>0, length(plot_list_model_selection) %/% 3+1, length(plot_list_model_selection) %/% 3)
pdf(glue::glue("../plots/5_sp_different_model_selection.pdf"), width = 8*3, height = 4*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = plot_list_model_selection, ncol = 3)  # Adjust 'ncol' to control the number of plots per row
dev.off()
pdf(glue::glue("../plots/5_sp_cor90.pdf"), width = 8*3, height = 4*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = plot_list_cor90, ncol = 3)  # Adjust 'ncol' to control the number of plots per row
dev.off()
pdf(glue::glue("../plots/5_sp_plot_list_pit_d_by_model_selection.pdf"), width = 8*3, height = 4*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = plot_list_pit_d_by_model_selection, ncol = 3)  # Adjust 'ncol' to control the number of plots per row
dev.off()
pdf(glue::glue("../plots/5_sp_plot_list_str_d_nso_d_by_model_selection.pdf"), width = 8*3, height = 4*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = plot_list_str_d_and_nso_d_by_model_selection, ncol = 3)  # Adjust 'ncol' to control the number of plots per row
dev.off()
pdf(glue::glue("../plots/5_sp_plot_list_str_d_and_speed_d_by_model_selection.pdf"), width = 8*3, height = 4*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = plot_list_str_d_and_speed_d_by_model_selection, ncol = 3)  # Adjust 'ncol' to control the number of plots per row
dev.off()

######## plot the radar plot
all_res <- do.call(rbind, all_res)
# all_res <- all_res[,c('sp','models','end_traverse_cor','nso_d','str_d','pit_d', 'weighted_mean_ll_improvement', 'weighted_mean_win_distance', 'weighted_energy_improvement', 'weighted_energy_improvement_days_integral',
#                       'weighted_energy_improvement_kms_integral')]
all_res <- all_res[,c('sp','models','end_traverse_cor','n_stopovers_diff','straightness_diff','speed_diff', 'pit_d', 'weighted_mean_ll_improvement', 'weighted_mean_win_distance', 'weighted_energy_improvement')]
all_res$n_stopovers_similarity <- -all_res$n_stopovers_diff
all_res$straightness_similarity <- -all_res$straightness_diff
all_res$speed_similarity <- -all_res$speed_diff
all_res <- all_res[,-which(names(all_res) %in% c("n_stopovers_diff",
                                     "straightness_diff",
                                     "speed_diff"))]

library(dplyr)
library(ggradar)
library(gridExtra)
library(scales)

### Norm for all models
plot_list <- list()
unique_sps <- unique(all_res$sp)
for (sp_value in unique_sps) {
  tmp_data <- all_res %>%
    filter(sp == sp_value, models != "nothing") %>%
    select(-sp)
  
  for (col in names(tmp_data)[-1]) {
    tmp_data[[col]] <- (tmp_data[[col]] - min(all_res[[col]])) / (max(all_res[[col]]) - min(all_res[[col]]))
  }
  
  new_names <- sapply(names(tmp_data)[-1], function(x) paste(strwrap(gsub('_',' ',x), width=10), collapse = '\n'))
  names(tmp_data)[-1] <- new_names
  
  p <- ggradar(tmp_data, group.point.size = 4, grid.label.size = 3) +
    guides(color = guide_legend(ncol = 2, bycol = TRUE)) +
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.position = "bottom"
    ) +
    ggtitle(paste("sp:", sp_value))
  
  plot_list[[sp_value]] <- p
}
pdf(glue::glue("../plots/5_sp_model_selection_radar_plot_norm_for_all_models.pdf"), width = 13*3, height = 11*2)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = plot_list, ncol = 3, nrow = 2)
dev.off()


### Norm just for just the best models
for (sp_value in unique_sps) {
  tmp_data <- all_res %>%
    filter(sp == sp_value, models != "nothing") %>%
    select(-sp)
  
  for (col in names(tmp_data)[-1]) {
    tmp_data[[col]] <- (tmp_data[[col]] - min(tmp_data[[col]])) / (max(tmp_data[[col]]) - min(tmp_data[[col]]))
  }
  
  new_names <- sapply(names(tmp_data)[-1], function(x) paste(strwrap(gsub('_',' ',x), width=10), collapse = '\n'))
  names(tmp_data)[-1] <- new_names
  
  p <- ggradar(tmp_data, group.point.size = 4, grid.label.size = 3) +
    guides(color = guide_legend(ncol = 2, bycol = TRUE)) +
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.position = "bottom"
    ) +
    ggtitle(paste("sp:", sp_value))
  
  plot_list[[sp_value]] <- p
}

n_row <- ifelse(length(plot_list_model_selection) %% 3>0, length(plot_list_model_selection) %/% 3+1, length(plot_list_model_selection) %/% 3)
pdf(glue::glue("../plots/5_sp_model_selection_radar_plot_norm_just_for_best_models.pdf"), width = 13*3, height = 11*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = plot_list, ncol = 3, nrow = 2)
dev.off()



######## Plot routes
load_eval_metrics <- function(sp){

  eval_metrics_list <- list()
  eval_metrics_count <- 1
  this_data_name='all_combined'
  # Load eval metrics
  eval_metrics_train_distance_metric_path <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_{date_}/eval_metrics_train_distance_metric_{this_data_name}.rds')
  eval_metrics_train_multi_objective_path <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_{date_}/eval_metrics_train_multi_objective_{this_data_name}.rds')
  eval_metrics_test_distance_metric_path <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_{date_}/eval_metrics_test_distance_metric_{this_data_name}.rds')
  
  if (!(file.exists(eval_metrics_train_distance_metric_path) & file.exists(eval_metrics_train_multi_objective_path) & file.exists(eval_metrics_test_distance_metric_path))){
    next
  }
  
  # 1
  eval_metrics_train_distance_metric <- readRDS(eval_metrics_train_distance_metric_path)
  eval_metrics_train_distance_metric$ll_improvement <- eval_metrics_train_distance_metric$mean_ll - eval_metrics_train_distance_metric$mean_null_ll
  eval_metrics_train_distance_metric$weighted_ll_improvement <- eval_metrics_train_distance_metric$weighted_mean_ll - eval_metrics_train_distance_metric$weighted_mean_null_ll
  eval_metrics_train_distance_metric <- eval_metrics_train_distance_metric[,c('model','weighted_mean_win_prob','weighted_mean_win_distance','weighted_ll_improvement',
                                                                              'area_win_distance_by_time', 'mean_win_distance',
                                                                              'area_win_prob_by_time','mean_win_prob', 'll_improvement',
                                                                              'end_traverse_cor', 'weighted_mean_effective_win_distance','weighted_energy_improvement',
                                                                              'weighted_energy_improvement_days_integral')]
  
  # Rename
  old_names <- c("weighted_mean_win_prob", "weighted_mean_win_distance", 
                 "weighted_ll_improvement", "area_win_distance_by_time",
                 "mean_win_distance", "area_win_prob_by_time", 
                 "mean_win_prob", "ll_improvement", 'weighted_mean_effective_win_distance','weighted_energy_improvement',
                 'weighted_energy_improvement_days_integral')
  new_names <- c(
    glue::glue("train_weighted_mean_win_prob_{this_data_name}"),
    glue::glue("train_weighted_mean_win_distance_{this_data_name}"),
    glue::glue("train_weighted_ll_improvement_{this_data_name}"),
    glue::glue("train_area_win_distance_by_time_{this_data_name}"),
    glue::glue("train_mean_win_distance_{this_data_name}"),
    glue::glue("train_area_win_prob_by_time_{this_data_name}"),
    glue::glue("train_mean_win_prob_{this_data_name}"),
    glue::glue("train_ll_improvement_{this_data_name}"),
    glue::glue("train_weighted_mean_effective_win_distance_{this_data_name}"),
    glue::glue("train_weighted_energy_improvement_{this_data_name}"),
    glue::glue("train_weighted_energy_improvement_days_integral_{this_data_name}")
  )
  col_index <- match(old_names, names(eval_metrics_train_distance_metric))
  names(eval_metrics_train_distance_metric)[col_index] <- new_names
  
  eval_metrics_list[[eval_metrics_count]] <- eval_metrics_train_distance_metric
  eval_metrics_count = eval_metrics_count+1
  
  # 2
  eval_metrics_train_multi_objective <- readRDS(eval_metrics_train_multi_objective_path)
  eval_metrics_train_multi_objective <- eval_metrics_train_multi_objective[,names(eval_metrics_train_multi_objective)[names(eval_metrics_train_multi_objective) %in% c('model','overall_des', 'pit_d', 'etc_d', 'str_d', 'nso_d', "pit_row", "pit_col", "pit_in_95")]]
  eval_metrics_train_multi_objective <- eval_metrics_train_multi_objective |> dplyr::rename('multi_objective_metric'='overall_des')
  
  old_names <- c("multi_objective_metric", "pit_d", "etc_d", "str_d", "nso_d", "pit_row", "pit_col", "pit_in_95")
  new_names <- c(
    glue::glue("train_multi_objective_metric_{this_data_name}"),
    glue::glue("train_pit_d_{this_data_name}"),
    glue::glue("train_etc_d_{this_data_name}"),
    glue::glue("train_str_d_{this_data_name}"),
    glue::glue("train_nso_d_{this_data_name}"),
    glue::glue("train_pit_row_{this_data_name}"),
    glue::glue("train_pit_col_{this_data_name}"),
    glue::glue("train_pit_in_95_{this_data_name}")
  )
  
  for (name_id in 1:length(old_names)){
    if (old_names[name_id] %in% names(eval_metrics_train_multi_objective)){
      names(eval_metrics_train_multi_objective)[which(names(eval_metrics_train_multi_objective)==old_names[name_id])] <- new_names[name_id]
    } else {
      eval_metrics_train_multi_objective[[new_names[name_id]]] <- c(NA)
    }
  }
  
  eval_metrics_list[[eval_metrics_count]] <- eval_metrics_train_multi_objective
  eval_metrics_count = eval_metrics_count+1
  
  # 3
  eval_metrics_test_distance_metric <- readRDS(eval_metrics_test_distance_metric_path)
  eval_metrics_test_distance_metric$ll_improvement <- eval_metrics_test_distance_metric$mean_ll - eval_metrics_test_distance_metric$mean_null_ll
  eval_metrics_test_distance_metric$weighted_ll_improvement <- eval_metrics_test_distance_metric$weighted_mean_ll - eval_metrics_test_distance_metric$weighted_mean_null_ll
  eval_metrics_test_distance_metric <- eval_metrics_test_distance_metric[,c('model','weighted_mean_win_prob','weighted_mean_win_distance','weighted_ll_improvement',
                                                                            'area_win_distance_by_time','mean_win_distance',
                                                                            'area_win_prob_by_time','mean_win_prob', 'll_improvement','weighted_mean_effective_win_distance',
                                                                            'weighted_energy_improvement', 'weighted_energy_improvement_days_integral')]
  old_names <- c(
    "weighted_mean_win_prob", "weighted_mean_win_distance", "weighted_ll_improvement",
    "area_win_distance_by_time", "mean_win_distance", "area_win_prob_by_time",
    "mean_win_prob", "ll_improvement", 'weighted_mean_effective_win_distance',
    'weighted_energy_improvement', 'weighted_energy_improvement_days_integral'
  )
  new_names <- c(
    glue::glue("test_weighted_mean_win_prob_{this_data_name}"),
    glue::glue("test_weighted_mean_win_distance_{this_data_name}"),
    glue::glue("test_weighted_ll_improvement_{this_data_name}"),
    glue::glue("test_area_win_distance_by_time_{this_data_name}"),
    glue::glue("test_mean_win_distance_{this_data_name}"),
    glue::glue("test_area_win_prob_by_time_{this_data_name}"),
    glue::glue("test_mean_win_prob_{this_data_name}"),
    glue::glue("test_ll_improvement_{this_data_name}"),
    glue::glue("test_weighted_mean_effective_win_distance_{this_data_name}"),
    glue::glue("test_weighted_energy_improvement_{this_data_name}"),
    glue::glue("test_weighted_energy_improvement_days_integral_{this_data_name}")
  )
  col_index <- match(old_names, names(eval_metrics_test_distance_metric))
  names(eval_metrics_test_distance_metric)[col_index] <- new_names
  
  eval_metrics_list[[eval_metrics_count]] <- eval_metrics_test_distance_metric
  eval_metrics_count = eval_metrics_count+1
  
  merged_df <- Reduce(function(x, y) merge(x, y, by = "model", all.x=T), eval_metrics_list)
  return(merged_df)
}


### 
plot_list <- list()
for (sp in c('amewoo', 'buwtea', 'swahaw', 'brwhaw', 'woothr', 'lobcur')){
  all_metrics <- load_eval_metrics(sp)
  best_by_ll_improvement <- all_metrics[
    order(-all_metrics$train_ll_improvement_all_combined)
    ,'model'][1]
  best_by_mean_win_distance <- all_metrics[
    order(-all_metrics$train_mean_win_distance_all_combined)
    ,'model'][1]
  best_by_multi_objective <- all_metrics[
    order(-all_metrics$train_multi_objective_metric_all_combined)
    ,'model'][1]
  best_by_weighted_mean_effective_win_distance <- all_metrics[
    order(-all_metrics$train_weighted_mean_effective_win_distance_all_combined)
    ,'model'][1]
  best_by_weighted_energy_improvement <- all_metrics[
    order(-all_metrics$train_weighted_energy_improvement_all_combined)
    ,'model'][1]
  best_by_weighted_energy_improvement_days_integral <- all_metrics[
    order(-all_metrics$train_weighted_energy_improvement_days_integral_all_combined)
    ,'model'][1]
  
  all_metrics <- all_metrics |> dplyr::mutate(score=scale(train_weighted_mean_win_distance_all_combined) + scale(train_weighted_ll_improvement_all_combined)) |> dplyr::arrange(-score)
  best_by_ST_threshold_090_ll_improvement_plus_distance <- all_metrics[all_metrics$end_traverse_cor>0.9,'model'][1]
  all_metrics <- all_metrics |> dplyr::arrange(-train_weighted_mean_effective_win_distance_all_combined)
  best_by_ST_threshold_090_weighted_mean_effective_win_distance <- all_metrics[all_metrics$end_traverse_cor>0.9,'model'][1]
  best_by_average_param_tracking <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/americas/{sp}_100km/{sp}_2022_100km_obs1.0_ent0.001924_dist0.008177_pow0.4167.hdf5')
  
  ###
  bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ll_improvement}"))
  tmp_bf <- bf
  routes <- route(bf, n=10)
  route_plot0 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by LL improvement"))
  plot_list[[length(plot_list) + 1]] <- route_plot0
  
  bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_mean_win_distance}"))
  routes <- route(bf, n=10)
  route_plot1 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by distance metric"))
  plot_list[[length(plot_list) + 1]] <- route_plot1
  
  bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_multi_objective}"))
  routes <- route(bf, n=10)
  route_plot2 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by Multi-objectives"))
  plot_list[[length(plot_list) + 1]] <- route_plot2
  
  bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_weighted_mean_effective_win_distance}"))
  routes <- route(bf, n=10)
  route_plot2 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by weighted effective mean win distance"))
  plot_list[[length(plot_list) + 1]] <- route_plot2
  
  bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ST_threshold_090_ll_improvement_plus_distance}"))
  routes <- route(bf, n=10)
  route_plot <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by 0.9 threshold end traverse and LL improvement and distance metric"))
  plot_list[[length(plot_list) + 1]] <- route_plot
  
  bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ST_threshold_090_weighted_mean_effective_win_distance}"))
  routes <- route(bf, n=10)
  route_plot <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by 0.9 threshold end traverse and weighted effective mean distance"))
  plot_list[[length(plot_list) + 1]] <- route_plot
  
  bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_weighted_energy_improvement}"))
  routes <- route(bf, n=10)
  route_plot <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by weighted_energy_improvement"))
  plot_list[[length(plot_list) + 1]] <- route_plot
  
  bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_weighted_energy_improvement_days_integral}"))
  routes <- route(bf, n=10)
  route_plot <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by weighted_energy_improvement_days_integral"))
  plot_list[[length(plot_list) + 1]] <- route_plot
  
  bf <- import_birdflow(best_by_average_param_tracking)
  routes <- route(bf, n=10)
  routes <- routes$data
  routes <- routes |> BirdFlowR::Routes(species = tmp_bf$species, metadata = tmp_bf$metadata) |> BirdFlowR::as_BirdFlowRoutes(bf=tmp_bf)
  route_plot3 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Averaged parameter (from the 6 sp)"))
  plot_list[[length(plot_list) + 1]] <- route_plot3
}

pdf(glue::glue("../plots/5_sp_routes_model_selection_tracking.pdf"), width = 6*9, height = 4*6)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = plot_list, ncol = 9, nrow = 6)
dev.off()



