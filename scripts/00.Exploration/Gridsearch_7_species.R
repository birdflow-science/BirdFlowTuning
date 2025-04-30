## Re-install BirdFlow & BirdFlowPipeline
# for (package in c('BirdFlowR','BirdFlowPipeline')){
#   if (package %in% installed.packages()) {
#     remove.packages(package)
#     # detach("package:BirdFlowR", unload = TRUE)
#   } else {
#     cat(paste0(package, " is not installed.\n"))
#   }
# }

devtools::install_local("/home/yc85_illinois_edu/BirdFlowR", force = T, dependencies = FALSE) # if the BirdFlowR is updated, we need to reinstall it, so that i can be used in BirdFlowPipeline!
devtools::install_local("/home/yc85_illinois_edu/BirdFlowPipeline", force = T, dependencies = FALSE)

##
library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

devtools::document("/home/yc85_illinois_edu/BirdFlowR")
devtools::document("/home/yc85_illinois_edu/BirdFlowPipeline")
load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.


#### Batch fit
species_list <- c('amewoo', 'buwtea', 'swahaw', 'brwhaw', 'woothr', 'lobcur') # c() #, , 'bkbplo', 'osprey', 'tunswa', 'turvul',  'whimbr', 'paibun', 'ovenbi1'

for (sp in species_list){
  sp_output_path <- paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric','/',sp)
  if (!dir.exists(sp_output_path)){dir.create(sp_output_path, recursive = TRUE)}
  
  tryCatch({
    batch_flow(sp, 
               gpu_ram = 10,
               hdf_path = sp_output_path,
               base_output_path = sp_output_path,
               model_selection = 'distance_metric')
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  })
}


# 
# ## Plot
# library(ggplot2)
# get_the_transition_pairs <- function(sp){
#   
#   model_files <- list.files(glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'))
#   model_files <- model_files[grepl(glue::glue("^{sp}_2022_150km_"), model_files)]
#   random_model <- BirdFlowR::import_birdflow(glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model_files[1]}'))
#   transition_df <- readRDS(
#     glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_2025-02-26/all_ground_truth_transitions_df.rds')
#   )
#   routes_obj <- BirdFlowR::Routes(transition_df, species=list(), metadata=random_model$metadata, source=source)
#   interval_obj <- routes_obj |> 
#     BirdFlowR::as_BirdFlowRoutes(bf=random_model,
#                                  aggregate = "random") |> 
#     BirdFlowR::as_BirdFlowIntervals(max_n=5000,
#                                     min_day_interval=7,
#                                     max_day_interval=180,
#                                     min_km_interval=200,
#                                     max_km_interval=8000)
#   set.seed(42)
#   train_data <- interval_obj$data |> dplyr::sample_frac(0.7)
#   # test_data <- dplyr::setdiff(interval_obj$data, train_data)
#   train_data <- BirdFlowIntervals(data=train_data,
#                                   species=interval_obj$species,
#                                   metadata=interval_obj$metadata,
#                                   geom=interval_obj$geom,
#                                   dates=interval_obj$dates,
#                                   source=interval_obj$source)
#   return(train_data)
# }
# 
# 
# plot_comparison <- function(plotting_data, x, y, title){
#   library(ggplot2)
#   plotting_data$point_color <- ifelse(plotting_data[[x]]==max(plotting_data[[x]]), 'red', 'black')
#   plotting_data$point_alpha <- ifelse(plotting_data[[x]]==max(plotting_data[[x]]), 1, 0.8)
#   plotting_data$point_size <- ifelse(plotting_data[[x]]==max(plotting_data[[x]]), 1, 0.8)
#   p <- ggplot2::ggplot(data=plotting_data) +
#     ggplot2::geom_point(ggplot2::aes(x=.data[[x]], y=.data[[y]], color=.data[['point_color']], alpha = point_alpha, size = point_size)) +
#     ggplot2::scale_color_manual(values = c("red" = "red", "black" = "black")) +
#     ggplot2::scale_size_continuous(range = c(4, 5), guide = "none") +
#     ggplot2::scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
#     ggplot2::theme_minimal() +
#     ggplot2::ggtitle(title) + 
#     theme(legend.position = "none")
#   return(p)
# }
# 
# 
# ##### plots_tuning_by_mean_win_distance
# 
# load_eval_metrics <- function(sp){
#   data_name <- list('banding_and_motus', 'tracking')
#   eval_metrics_list <- list()
#   eval_metrics_count <- 1
#   for (this_data_name in data_name){
#     # Load eval metrics
#     eval_metrics_train_distance_metric_path <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_2025-03-08/eval_metrics_train_distance_metric_{this_data_name}.rds')
#     eval_metrics_train_multi_objective_path <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_2025-03-08/eval_metrics_train_multi_objective_{this_data_name}.rds')
#     eval_metrics_test_distance_metric_path <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km_2025-03-08/eval_metrics_test_distance_metric_{this_data_name}.rds')
#     
#     if (!(file.exists(eval_metrics_train_distance_metric_path) & file.exists(eval_metrics_train_multi_objective_path) & file.exists(eval_metrics_test_distance_metric_path))){
#       next
#     }
#     
#     # 1
#     eval_metrics_train_distance_metric <- readRDS(eval_metrics_train_distance_metric_path)
#     eval_metrics_train_distance_metric$ll_improvement <- eval_metrics_train_distance_metric$mean_ll - eval_metrics_train_distance_metric$mean_null_ll
#     eval_metrics_train_distance_metric$weighted_ll_improvement <- eval_metrics_train_distance_metric$weighted_mean_ll - eval_metrics_train_distance_metric$weighted_mean_null_ll
#     eval_metrics_train_distance_metric <- eval_metrics_train_distance_metric[,c('model','weighted_mean_win_prob','weighted_mean_win_distance','weighted_ll_improvement',
#                                                                                'area_win_distance_by_time', 'mean_win_distance',
#                                                                                'area_win_prob_by_time','mean_win_prob', 'll_improvement',
#                                                                                'end_traverse_cor',
#                                                                                'mean_ELC_distance', 'weighted_mean_ELC_distance',
#                                                                                'mean_RELC_distance', 'weighted_mean_RELC_distance')]
#     
#     # Rename
#     old_names <- c("weighted_mean_win_prob", "weighted_mean_win_distance", 
#                    "weighted_ll_improvement", "area_win_distance_by_time",
#                    "mean_win_distance", "area_win_prob_by_time", 
#                    "mean_win_prob", "ll_improvement",
#                    'mean_ELC_distance','weighted_mean_ELC_distance',
#                    'mean_RELC_distance', 'weighted_mean_RELC_distance')
#     new_names <- c(
#       glue::glue("train_weighted_mean_win_prob_{this_data_name}"),
#       glue::glue("train_weighted_mean_win_distance_{this_data_name}"),
#       glue::glue("train_weighted_ll_improvement_{this_data_name}"),
#       glue::glue("train_area_win_distance_by_time_{this_data_name}"),
#       glue::glue("train_mean_win_distance_{this_data_name}"),
#       glue::glue("train_area_win_prob_by_time_{this_data_name}"),
#       glue::glue("train_mean_win_prob_{this_data_name}"),
#       glue::glue("train_ll_improvement_{this_data_name}"),
#       glue::glue("train_mean_ELC_distance_{this_data_name}"),
#       glue::glue("train_weighted_mean_ELC_distance_{this_data_name}"),
#       glue::glue("train_mean_RELC_distance_{this_data_name}"),
#       glue::glue("train_weighted_mean_RELC_distance_{this_data_name}")
#     )
#     col_index <- match(old_names, names(eval_metrics_train_distance_metric))
#     names(eval_metrics_train_distance_metric)[col_index] <- new_names
#     
#     eval_metrics_list[[eval_metrics_count]] <- eval_metrics_train_distance_metric
#     eval_metrics_count = eval_metrics_count+1
#     
#     # 2
#     eval_metrics_train_multi_objective <- readRDS(eval_metrics_train_multi_objective_path)
#     eval_metrics_train_multi_objective <- eval_metrics_train_multi_objective[,names(eval_metrics_train_multi_objective)[names(eval_metrics_train_multi_objective) %in% c('model','overall_des', 'pit_d', 'etc_d', 'str_d', 'nso_d', "pit_row", "pit_col", "pit_in_95")]]
#     eval_metrics_train_multi_objective <- eval_metrics_train_multi_objective |> dplyr::rename('multi_objective_metric'='overall_des')
#     
#     old_names <- c("multi_objective_metric", "pit_d", "etc_d", "str_d", "nso_d", "pit_row", "pit_col", "pit_in_95")
#     new_names <- c(
#       glue::glue("train_multi_objective_metric_{this_data_name}"),
#       glue::glue("train_pit_d_{this_data_name}"),
#       glue::glue("train_etc_d_{this_data_name}"),
#       glue::glue("train_str_d_{this_data_name}"),
#       glue::glue("train_nso_d_{this_data_name}"),
#       glue::glue("train_pit_row_{this_data_name}"),
#       glue::glue("train_pit_col_{this_data_name}"),
#       glue::glue("train_pit_in_95_{this_data_name}")
#     )
#     
#     for (name_id in 1:length(old_names)){
#       if (old_names[name_id] %in% names(eval_metrics_train_multi_objective)){
#         names(eval_metrics_train_multi_objective)[which(names(eval_metrics_train_multi_objective)==old_names[name_id])] <- new_names[name_id]
#       } else {
#         eval_metrics_train_multi_objective[[new_names[name_id]]] <- c(NA)
#       }
#     }
# 
#     eval_metrics_list[[eval_metrics_count]] <- eval_metrics_train_multi_objective
#     eval_metrics_count = eval_metrics_count+1
#     
#     # 3
#     eval_metrics_test_distance_metric <- readRDS(eval_metrics_test_distance_metric_path)
#     eval_metrics_test_distance_metric$ll_improvement <- eval_metrics_test_distance_metric$mean_ll - eval_metrics_test_distance_metric$mean_null_ll
#     eval_metrics_test_distance_metric$weighted_ll_improvement <- eval_metrics_test_distance_metric$weighted_mean_ll - eval_metrics_test_distance_metric$weighted_mean_null_ll
#     eval_metrics_test_distance_metric <- eval_metrics_test_distance_metric[,c('model','weighted_mean_win_prob','weighted_mean_win_distance','weighted_ll_improvement',
#                                          'area_win_distance_by_time','mean_win_distance',
#                                          'area_win_prob_by_time','mean_win_prob', 'll_improvement',
#                                          'mean_ELC_distance', 'weighted_mean_ELC_distance',
#                                          'mean_RELC_distance', 'weighted_mean_RELC_distance')]
#     old_names <- c(
#       "weighted_mean_win_prob", "weighted_mean_win_distance", "weighted_ll_improvement",
#       "area_win_distance_by_time", "mean_win_distance", "area_win_prob_by_time",
#       "mean_win_prob", "ll_improvement",
#       'mean_ELC_distance', 'weighted_mean_ELC_distance',
#       'mean_RELC_distance', 'weighted_mean_RELC_distance'
#       
#     )
#     new_names <- c(
#       glue::glue("test_weighted_mean_win_prob_{this_data_name}"),
#       glue::glue("test_weighted_mean_win_distance_{this_data_name}"),
#       glue::glue("test_weighted_ll_improvement_{this_data_name}"),
#       glue::glue("test_area_win_distance_by_time_{this_data_name}"),
#       glue::glue("test_mean_win_distance_{this_data_name}"),
#       glue::glue("test_area_win_prob_by_time_{this_data_name}"),
#       glue::glue("test_mean_win_prob_{this_data_name}"),
#       glue::glue("test_ll_improvement_{this_data_name}"),
#       glue::glue("test_mean_ELC_distance_{this_data_name}"),
#       glue::glue("test_weighted_mean_ELC_distance_{this_data_name}"),
#       glue::glue("test_mean_RELC_distance_{this_data_name}"),
#       glue::glue("test_weighted_mean_RELC_distance_{this_data_name}")
#     )
#     col_index <- match(old_names, names(eval_metrics_test_distance_metric))
#     names(eval_metrics_test_distance_metric)[col_index] <- new_names
#     
#     eval_metrics_list[[eval_metrics_count]] <- eval_metrics_test_distance_metric
#     eval_metrics_count = eval_metrics_count+1
#   }
#   
#   merged_df <- Reduce(function(x, y) merge(x, y, by = "model", all.x=T), eval_metrics_list)
#   return(merged_df)
# }
# 
# all_metrics_list <- list()
# for (sp in c('amewoo')){ #, 'buwtea', 'lobcur', 'swahaw', 'brwhaw', 'woothr'
#   all_metrics_list[[sp]] <- load_eval_metrics(sp)
# }
# 
# test <- load_eval_metrics('amewoo')
# 
# plot(test$test_area_win_distance_by_time_banding_and_motus, test$test_ll_improvement_banding_and_motus)
# points(test[test$model=='amewoo_2022_150km_obs1.0_ent0.001443_dist0.008658_pow0.6.hdf5',]$test_area_win_distance_by_time_banding_and_motus, 
#        test[test$model=='amewoo_2022_150km_obs1.0_ent0.001443_dist0.008658_pow0.6.hdf5',]$test_ll_improvement_banding_and_motus, col='red', pch=20)
# 
# plot(test$train_area_win_distance_by_time_banding_and_motus, test$train_ll_improvement_banding_and_motus)
# points(test[test$model=='amewoo_2022_150km_obs1.0_ent0.001443_dist0.008658_pow0.6.hdf5',]$train_area_win_distance_by_time_banding_and_motus, 
#        test[test$model=='amewoo_2022_150km_obs1.0_ent0.001443_dist0.008658_pow0.6.hdf5',]$train_ll_improvement_banding_and_motus, col='red', pch=20)
# 
# plot(test$train_pit_row_tracking, test$train_pit_col_tracking)
# points(test[test$model=='amewoo_2022_150km_obs1.0_ent0.001443_dist0.008658_pow0.6.hdf5',]$train_pit_row_tracking, 
#        test[test$model=='amewoo_2022_150km_obs1.0_ent0.001443_dist0.008658_pow0.6.hdf5',]$train_pit_col_tracking, col='red', pch=20)
# 
# plot(test$end_traverse_cor.x, test$train_pit_d_tracking)
# top5_mdoel_by_test_weighted_mean_win_distance_tracking <- test[order(-test$test_weighted_mean_win_distance_tracking),]$model[1:5]
# points(test[test$model %in% top5_mdoel_by_test_weighted_mean_win_distance_tracking,]$end_traverse_cor.x, 
#        test[test$model %in% top5_mdoel_by_test_weighted_mean_win_distance_tracking,]$train_pit_d_tracking, col='red', pch=20)
# 
# plot(test$end_traverse_cor.x, test$train_pit_d_tracking)
# top5_mdoel_by_test_weighted_mean_win_distance_tracking <- test[order(-test$test_mean_win_distance_tracking),]$model[1:5]
# points(test[test$model %in% top5_mdoel_by_test_weighted_mean_win_distance_tracking,]$end_traverse_cor.x, 
#        test[test$model %in% top5_mdoel_by_test_weighted_mean_win_distance_tracking,]$train_pit_d_tracking, col='red', pch=20)
# 
# 
# # tuning_list <- c('train_weighted_mean_win_prob','train_weighted_mean_win_distance',
# #                  'train_mean_win_prob', 'train_mean_win_distance', 'train_weighted_ll_improvement', 'train_ll_improvement')
# # test_list <- c('test_weighted_mean_win_prob','test_weighted_mean_win_distance',
# #                'test_mean_win_distance','test_mean_win_prob', 'test_weighted_ll_improvement','test_ll_improvement')
# 
# tuning_list <- c('train_mean_win_distance', 'train_ll_improvement', 'train_multi_objective_metric')
# test_list <- c('test_mean_win_distance', 'test_ll_improvement', 'end_traverse_cor.x', 'train_pit_d')
# 
# 
# plot_list <- list()
# for (tune_on in tuning_list){
#   for (test_on in test_list){
#     data_name <- list('banding_and_motus', 'tracking')
#     for (this_data_name in data_name){
#       plot_name <- paste0(tune_on,'___',test_on,'___',this_data_name)
#       
#       all_plots_for_the_same_pdf <- list()
#       
#       sp_count <- 1
#       for (sp in c('amewoo', 'buwtea', 'swahaw', 'brwhaw', 'woothr')){
#         if (!sp %in% names(all_metrics_list)){
#           next
#         }
#         
#         plotting_data <- all_metrics_list[[sp]]
#         x <- glue::glue('{tune_on}_{this_data_name}')
#         if (test_on=='end_traverse_cor.x'){
#           y=test_on
#         } else {
#           y <- glue::glue('{test_on}_{this_data_name}')
#         }
#         
#         title <- paste0(sp, '\n', tune_on, '\n', test_on,'\n', this_data_name)
#         p <- plot_comparison(plotting_data, x, y, title)
#         
#         all_plots_for_the_same_pdf[[sp_count]] <- p
#         sp_count <- sp_count + 1
#       }
#       
#       plot_list[[plot_name]] <- all_plots_for_the_same_pdf
#     }
#   }   
# }
# 
# # Another one
# tune_on = 'train_mean_win_distance_tracking'
# test_on = 'test_mean_win_distance_banding_and_motus'
# plot_name <- paste0(tune_on,'___',test_on)
# all_plots_for_the_same_pdf <- list()
# sp_count <- 1
# for (sp in c('amewoo', 'buwtea', 'swahaw', 'brwhaw', 'woothr')){
#   if (!sp %in% names(all_metrics_list)){
#     next
#   }
#   
#   plotting_data <- all_metrics_list[[sp]]
#   x <- tune_on
#   if (test_on=='end_traverse_cor.x'){
#     y=test_on
#   } else {
#     y <- test_on
#   }
#   
#   title <- paste0(sp, '\n', tune_on, '\n', test_on)
#   p <- plot_comparison(plotting_data, x, y, title)
#   
#   all_plots_for_the_same_pdf[[sp_count]] <- p
#   sp_count <- sp_count + 1
# }
# plot_list[[plot_name]] <- all_plots_for_the_same_pdf
# 
# 
# dir_path <- glue::glue('../plots')
# if (!dir.exists(dir_path)) {
#   dir.create(dir_path, recursive = TRUE)
# }
# 
# for (plot_name in names(plot_list)){
#   pdf(glue::glue("../plots/{plot_name}.pdf"), width = 6*3, height = 4*2)  # Adjust PDF dimensions as needed
#   gridExtra::grid.arrange(grobs = plot_list[[plot_name]], ncol = 3)  # Adjust 'ncol' to control the number of plots per row
#   dev.off()
# }
# 
# 
# ##### Plot
# species <- 'amewoo'
# sp <- 'amewoo'
# bf <- BirdFlowR::import_birdflow('/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/americas/amewoo_100km/amewoo_2022_100km_obs1.0_ent0.001924_dist0.008177_pow0.4167.hdf5')
# routes <- route(bf, n=100)
# mean((routes$data[routes$data$stay_len>7,] |> dplyr::group_by(route_id) |> dplyr::mutate(stopover_cpunt = dplyr::n_distinct(stay_id)) |> dplyr::slice(1) |> dplyr::ungroup())$stopover_cpunt)
# 
# params <- set_pipeline_params(species = species)
# params <- preprocess_species_wrapper(params)
# track_birdflowroutes_obj <- get_real_track(bf, params) # Real trac
# routes <- track_birdflowroutes_obj
# mean((routes$data[routes$data$stay_len>7,] |> dplyr::group_by(route_id) |> dplyr::mutate(stopover_cpunt = dplyr::n_distinct(stay_id)) |> dplyr::slice(1) |> dplyr::ungroup())$stopover_cpunt)
# 
# ### Tracking
# track_birdflowroutes_obj <- get_real_track(bf, params) # Real track
# 
# plot_routes(routes)
# 
# 
# # bf <- BirdFlowR::import_birdflow('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/amewoo_2022_150km_obs1.0_ent0.008547_dist0.017094_pow0.7.hdf5') # selected by ll
# # bf <- BirdFlowR::import_birdflow('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/amewoo_2022_150km_obs1.0_ent0.008547_dist0.017094_pow0.9.hdf5') # selected by the multi objective function
# 
# get_each_transition_evaluation <- function(bf, title){
#   species = 'amewoo'
#   params <- set_pipeline_params(species = species)
#   params <- preprocess_species_wrapper(params)
#   
#   ### Tracking
#   track_birdflowroutes_obj <- get_real_track(bf, params) # Real track
#   track_interval_obj <- track_birdflowroutes_obj |> 
#     BirdFlowR::as_BirdFlowIntervals(max_n=5000,
#                                     min_day_interval=1,
#                                     max_day_interval=180,
#                                     min_km_interval=0,
#                                     max_km_interval=8000)
#   
#   interval_based_metrics_tracking <- get_interval_based_metrics(track_interval_obj, bf)
#   each_transition_tracking <- interval_based_metrics_tracking[[2]]
#   each_transition_tracking <- cbind(track_interval_obj$data, each_transition_tracking)
#   each_transition_tracking$ll_improvement <- each_transition_tracking$ll - each_transition_tracking$null_ll
#   
#   plot(each_transition_tracking$elapsed_days/7, each_transition_tracking$ll_improvement, 
#        xlab = "Elapsed Week", ylab='ALL improvement', main='Tracking data for amewoo')
#   
#   ggplot(each_transition_tracking, aes(x = elapsed_days/7, y = ll_improvement)) +
#     geom_point(alpha=0.5) +
#     geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
#     geom_smooth(method = "loess", se = FALSE, color = "blue", span=0.1) + 
#     ggtitle(file) +
#     theme(plot.title = element_text(size = 10))
#   
#   # each_transition_tracking <- read.csv('/home/yc85_illinois_edu/each_transition_tracking.csv')
#   write.csv(each_transition_tracking, glue::glue('/home/yc85_illinois_edu/each_transition_tracking_{title}.csv'), row.names = FALSE)
#   
#   ## Banding
#   banding_df <- load_banding_transitions_df(file.path(BirdFlowPipeline:::the$banding_rds_path, paste0(params$species, '.rds')))
#   banding_birdflowroutes_obj <- banding_df |> 
#     BirdFlowR::Routes(species=bf$species, metadata=params$metadata, source=NULL) |>
#     BirdFlowR::as_BirdFlowRoutes(bf=bf)
#   banding_interval_obj <-  banding_birdflowroutes_obj |>
#     BirdFlowR::as_BirdFlowIntervals(max_n=10000,
#                                     min_day_interval=1,
#                                     max_day_interval=180,
#                                     min_km_interval=0,
#                                     max_km_interval=8000)
#   interval_based_metrics_banding <- get_interval_based_metrics(banding_interval_obj, bf)
#   each_transition_banding <- interval_based_metrics_banding[[2]]
#   plot(each_transition_banding$elapsed_days/7, each_transition_banding$ll - each_transition_banding$null_ll,
#        xlab = "Elapsed Week", ylab='ALL improvement', main='Banding data for amewoo')
#   each_transition_banding <- cbind(banding_interval_obj$data, each_transition_banding)
#   each_transition_banding$ll_improvement <- each_transition_banding$ll - each_transition_banding$null_ll
#   # each_transition_banding <- read.csv('/home/yc85_illinois_edu/each_transition_banding.csv')
#   write.csv(each_transition_banding, glue::glue('/home/yc85_illinois_edu/each_transition_banding_{title}.csv'), row.names = FALSE)
#   
#   ## MOTUS
#   motus_df <- load_motus_transitions_df(file.path(BirdFlowPipeline:::the$motus_rds_path, paste0(params$species, '.rds')))
#   motus_df <- na.omit(motus_df)
#   motus_birdflowroutes_obj <- motus_df |> 
#     BirdFlowR::Routes(species=bf$species, metadata=params$metadata, source=NULL) |>
#     BirdFlowR::as_BirdFlowRoutes(bf=bf)
#   motus_interval_obj <-  motus_birdflowroutes_obj |>
#     BirdFlowR::as_BirdFlowIntervals(max_n=10000,
#                                     min_day_interval=1,
#                                     max_day_interval=180,
#                                     min_km_interval=0,
#                                     max_km_interval=8000)
#   interval_based_metrics_motus <- get_interval_based_metrics(motus_interval_obj, bf)
#   each_transition_motus <- interval_based_metrics_motus[[2]]
#   plot(each_transition_motus$elapsed_days/7, each_transition_motus$ll - each_transition_motus$null_ll,
#        xlab = "Elapsed Week", ylab='ALL improvement', main='MOTUS data for amewoo')
#   each_transition_motus <- cbind(motus_interval_obj$data, each_transition_motus)
#   each_transition_motus$ll_improvement <- each_transition_motus$ll - each_transition_motus$null_ll
#   # each_transition_motus <- read.csv('/home/yc85_illinois_edu/each_transition_motus.csv')
#   write.csv(each_transition_motus, glue::glue('/home/yc85_illinois_edu/each_transition_motus_{title}.csv'), row.names = FALSE)
#   
#   return(list(each_transition_tracking, each_transition_banding, each_transition_motus))
# }
# 
# 
# each_transition_from_best_model_by_multi_objectives <- get_each_transition_evaluation(
#   bf = BirdFlowR::import_birdflow(
#     paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/', 
#            all_metrics_list[['amewoo']][order(-all_metrics_list[['amewoo']]$train_multi_objective_metric_tracking), 'model'][1])
#   ),
#   title='model_selection_by_multi_objectives'
# )
# 
# each_transition_from_best_model_by_ll_improvement <- get_each_transition_evaluation(
#   bf = BirdFlowR::import_birdflow(
#     paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/', 
#            all_metrics_list[['amewoo']][order(-all_metrics_list[['amewoo']]$train_ll_improvement_tracking), 'model'][1])
#   ),
#   title='model_selection_by_ll_improvement'
# )
# 
# each_transition_from_best_model_by_mean_win_distance <- get_each_transition_evaluation(
#   bf = BirdFlowR::import_birdflow(
#     paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/', 
#            all_metrics_list[['amewoo']][order(-all_metrics_list[['amewoo']]$train_mean_win_distance_tracking), 'model'][1])
#   ),
#   title='model_selection_by_distance_metrics'
# )
# 
# 
# each_transition_from_best_by_ST_threshold_090_ll_imrpovement <- get_each_transition_evaluation(
#   bf = BirdFlowR::import_birdflow(
#     paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/', 
#            all_metrics_list[['amewoo']][all_metrics_list[['amewoo']]$end_traverse_cor.x>0.9,][order(-all_metrics_list[['amewoo']][all_metrics_list[['amewoo']]$end_traverse_cor.x>0.9,]$train_ll_improvement_tracking), 'model'][1])
#   ),
#   title='model_selection_by_ST_threshold_090_ll_imrpovement'
# )
# 
# each_transition_from_best_by_ST_threshold_090_mean_win_distance <- get_each_transition_evaluation(
#   bf = BirdFlowR::import_birdflow(
#     paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/', 
#            all_metrics_list[['amewoo']][all_metrics_list[['amewoo']]$end_traverse_cor.x>0.9,][order(-all_metrics_list[['amewoo']][all_metrics_list[['amewoo']]$end_traverse_cor.x>0.9,]$train_mean_win_distance_tracking), 'model'][1])
#   ),
#   title='model_selection_by_ST_threshold_090_mean_distance'
# )
# 
# tmp <- all_metrics_list[['amewoo']][all_metrics_list[['amewoo']]$end_traverse_cor.x>0.95,]
# tmp$des <- scale(tmp$train_mean_win_distance_tracking) + scale(tmp$train_ll_improvement_tracking)
# each_transition_from_best_by_ST_threshold_090_ll_imrpovement_and_mean_win_distance <- get_each_transition_evaluation(
#   bf = BirdFlowR::import_birdflow(
#     paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/', 
#            tmp[order(-tmp$des), 'model'][1])
#   ),
#   title='model_selection_by_ST_threshold_090_ll_imrpovement_and_mean_win_distance'
# )
# 
# 
# each_transition_from_best_model_by_weighted_mean_ELC_distance <- get_each_transition_evaluation(
#   bf = BirdFlowR::import_birdflow(
#     paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/', 
#            all_metrics_list[['amewoo']][order(-all_metrics_list[['amewoo']]$train_weighted_mean_ELC_distance_tracking), 'model'][1])
#   ),
#   title='model_selection_by_weighted_mean_ELC_distance'
# )
# 
# each_transition_from_best_model_by_weighted_mean_RELC_distance <- get_each_transition_evaluation(
#   bf = BirdFlowR::import_birdflow(
#     paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/', 
#            all_metrics_list[['amewoo']][order(-all_metrics_list[['amewoo']]$train_weighted_mean_RELC_distance_tracking), 'model'][1])
#   ),
#   title='model_selection_by_weighted_mean_RELC_distance'
# )
# 
# 
# each_transition_from_best_by_average_param_tracking <- get_each_transition_evaluation(
#   bf = BirdFlowR::import_birdflow(glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/americas/{sp}_100km/{sp}_2022_100km_obs1.0_ent0.001924_dist0.008177_pow0.4167.hdf5')),
#   title='model_selection_by_averaged_parameters'
# )
# 
# 
# #
# plot_poly_ggplot <- function(data, x, y, title, yintercept){
#   library(ggplot2)
#   library(dplyr)
#   
#   # # Fit polynomial models separately for each model selection group
#   # model_fits <- data %>%
#   #   dplyr::group_by(`model_selection`) %>%
#   #   do(model = lm(as.formula(paste(y, "~ poly(", x, ", 4, raw = TRUE)")), data = .))
#   # 
#   # # Generate predictions for each model selection group
#   # pred_data <- model_fits %>%
#   #   dplyr::rowwise() %>%
#   #   dplyr::mutate(
#   #     x_pred = list(seq(min(data[[x]]), max(data[[x]]), length.out = 200)),
#   #     y_pred = list(predict(model, newdata = setNames(data.frame(x_pred = x_pred), x)))
#   #   ) %>%
#   #   tidyr::unnest(cols = c(x_pred, y_pred)) %>%
#   #   dplyr::select(`model_selection`, x_pred, y_pred)
#   # 
#   # # Plot
#   # ggplot() +
#   #   # geom_point(data = data, aes(x = .data[[x]], y = .data[[y]], color = factor(`model_selection`)), alpha = 0.2, size=1) +
#   #   geom_line(data = pred_data, aes(x = x_pred, y = y_pred, group = factor(`model_selection`)), 
#   #             color = "black", linewidth = 1) +
#   #   geom_line(data = pred_data, aes(x = x_pred, y = y_pred, color = factor(`model_selection`)), linewidth = 0.5) +
#   #   geom_hline(yintercept = yintercept, color = "red", linetype = "dashed", linewidth = 1) +
#   #   labs(title = title, x = x, y = y, color = "Model Selection") +
#   #   theme_minimal() +
#   #   scale_color_brewer(palette = "Dark2")  # Adjust color palette for better visibility
#   # 
#   ggplot(data, aes(x = .data[[x]], y = .data[[y]], color = factor(`model_selection`))) +
#     # geom_point(alpha = 0.2, size = 1) +
#     geom_smooth(method = "loess", se = FALSE, linewidth = 0.5, span=0.7) +
#     geom_hline(yintercept = yintercept, color = "red", linetype = "dashed", linewidth = 0.5, alpha=0.7) +
#     labs(title = title, x = x, y = y, color = "Model Selection") +
#     theme_minimal()
#   
# }
# 
# # Create the plots
# all_transitions <- list()
# for (eval_metric in c('ll_improvement','win_distance_fraction','win_prob','win_distance', 'ELC_distance', 'RELC_distance')){
#   if (eval_metric=='win_prob'){
#     yintercept = 0.5
#   } else {
#     yintercept = 0
#   }
#   
#   tmp1 <- each_transition_from_best_model_by_multi_objectives[[1]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp1$model_selection <- 'Multi-objective'
#   tmp2 <- each_transition_from_best_model_by_ll_improvement[[1]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp2$model_selection <- 'LL improvement'
#   tmp3 <- each_transition_from_best_model_by_mean_win_distance[[1]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp3$model_selection <- 'Mean win distance'
#   tmp4 <- each_transition_from_best_by_ST_threshold_090_ll_imrpovement[[1]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp4$model_selection <- 'S&T90 Threshold + LL improvement'
#   tmp5 <- each_transition_from_best_by_ST_threshold_090_mean_win_distance[[1]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp5$model_selection <- 'S&T90 Threshold + Mean win distance'
#   tmp6 <- each_transition_from_best_by_average_param_tracking[[1]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp6$model_selection <- 'Averaged param model'
#   tmp7 <- each_transition_from_best_model_by_weighted_mean_ELC_distance[[1]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp7$model_selection <- 'Weighted mean ELC distance'
#   tmp8 <- each_transition_from_best_model_by_weighted_mean_RELC_distance[[1]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp8$model_selection <- 'Weighted mean RELC distance'
#   tmp9 <- each_transition_from_best_by_ST_threshold_090_ll_imrpovement_and_mean_win_distance[[1]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp9$model_selection <- 'S&T90 Threshold + LL improvement + Mean win distance'
#   
#   each_transition_tracking <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9)
#   each_transition_tracking$model_selection <- factor(each_transition_tracking$model_selection, levels = c("Multi-objective", 'LL improvement', 'Mean win distance', 'S&T90 Threshold + LL improvement', 'S&T90 Threshold + Mean win distance', 'Averaged param model', 'Weighted mean ELC distance', 'Weighted mean RELC distance', 'S&T90 Threshold + LL improvement + Mean win distance'))
#   each_transition_tracking <- na.omit(each_transition_tracking)
#   tmp <- each_transition_tracking
#   tmp$eval_metric <- eval_metric
#   names(tmp)[names(tmp)==eval_metric] <- 'value'
#   all_transitions[[length(all_transitions)+1]] <- tmp
#   
#   tmp1 <- each_transition_from_best_model_by_multi_objectives[[2]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp1$model_selection <- 'Multi-objective'
#   tmp2 <- each_transition_from_best_model_by_ll_improvement[[2]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp2$model_selection <- 'LL improvement'
#   tmp3 <- each_transition_from_best_model_by_mean_win_distance[[2]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp3$model_selection <- 'Mean win distance'
#   tmp4 <- each_transition_from_best_by_ST_threshold_090_ll_imrpovement[[2]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp4$model_selection <- 'S&T90 Threshold + LL improvement'
#   tmp5 <- each_transition_from_best_by_ST_threshold_090_mean_win_distance[[2]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp5$model_selection <- 'S&T90 Threshold + Mean win distance'
#   tmp6 <- each_transition_from_best_by_average_param_tracking[[2]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp6$model_selection <- 'Averaged param model'
#   tmp7 <- each_transition_from_best_model_by_weighted_mean_ELC_distance[[2]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp7$model_selection <- 'Weighted mean ELC distance'
#   tmp8 <- each_transition_from_best_model_by_weighted_mean_RELC_distance[[2]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp8$model_selection <- 'Weighted mean RELC distance'
#   tmp9 <- each_transition_from_best_by_ST_threshold_090_ll_imrpovement_and_mean_win_distance[[2]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp9$model_selection <- 'S&T90 Threshold + LL improvement + Mean win distance'
#   each_transition_motus <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9)
#   each_transition_motus$model_selection <- factor(each_transition_motus$model_selection, levels = c("Multi-objective", 'LL improvement', 'Mean win distance', 'S&T90 Threshold + LL improvement', 'S&T90 Threshold + Mean win distance', 'Averaged param model', 'Weighted mean ELC distance', 'Weighted mean RELC distance', 'S&T90 Threshold + LL improvement + Mean win distance'))
#   each_transition_motus <- na.omit(each_transition_motus)
#   tmp <- each_transition_motus
#   tmp$eval_metric <- eval_metric
#   names(tmp)[names(tmp)==eval_metric] <- 'value'
#   all_transitions[[length(all_transitions)+1]] <- tmp
#   
#   tmp1 <- each_transition_from_best_model_by_multi_objectives[[3]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp1$model_selection <- 'Multi-objective'
#   tmp2 <- each_transition_from_best_model_by_ll_improvement[[3]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp2$model_selection <- 'LL improvement'
#   tmp3 <- each_transition_from_best_model_by_mean_win_distance[[3]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp3$model_selection <- 'Mean win distance'
#   tmp4 <- each_transition_from_best_by_ST_threshold_090_ll_imrpovement[[3]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp4$model_selection <- 'S&T90 Threshold + LL improvement'
#   tmp5 <- each_transition_from_best_by_ST_threshold_090_mean_win_distance[[3]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp5$model_selection <- 'S&T90 Threshold + Mean win distance'
#   tmp6 <- each_transition_from_best_by_average_param_tracking[[3]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp6$model_selection <- 'Averaged param model'
#   tmp7 <- each_transition_from_best_model_by_weighted_mean_ELC_distance[[3]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp7$model_selection <- 'Weighted mean ELC distance'
#   tmp8 <- each_transition_from_best_model_by_weighted_mean_RELC_distance[[3]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp8$model_selection <- 'Weighted mean RELC distance'
#   tmp9 <- each_transition_from_best_by_ST_threshold_090_ll_imrpovement_and_mean_win_distance[[3]][,c('elapsed_km','elapsed_days',eval_metric)]
#   tmp9$model_selection <- 'S&T90 Threshold + LL improvement + Mean win distance'
#   each_transition_banding <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9)
#   each_transition_banding$model_selection <- factor(each_transition_banding$model_selection, levels = c("Multi-objective", 'LL improvement', 'Mean win distance', 'S&T90 Threshold + LL improvement', 'S&T90 Threshold + Mean win distance', 'Averaged param model', 'Weighted mean ELC distance', 'Weighted mean RELC distance', 'S&T90 Threshold + LL improvement + Mean win distance'))
#   each_transition_banding <- na.omit(each_transition_banding)
#   tmp <- each_transition_banding
#   tmp$eval_metric <- eval_metric
#   names(tmp)[names(tmp)==eval_metric] <- 'value'
#   all_transitions[[length(all_transitions)+1]] <- tmp
#   
#   p1 <- plot_poly_ggplot(each_transition_tracking, 'elapsed_km', eval_metric, 'tracking data', yintercept)
#   p2 <- plot_poly_ggplot(each_transition_motus, 'elapsed_km', eval_metric, 'motus data', yintercept)
#   p3 <- plot_poly_ggplot(each_transition_banding, 'elapsed_km', eval_metric, 'banding data', yintercept)
#   p4 <- plot_poly_ggplot(each_transition_tracking, 'elapsed_days', eval_metric, 'tracking data', yintercept)
#   p5 <- plot_poly_ggplot(each_transition_motus, 'elapsed_days', eval_metric, 'motus data', yintercept)
#   p6 <- plot_poly_ggplot(each_transition_banding, 'elapsed_days', eval_metric, 'banding data', yintercept)
#   
#   # Arrange the plots in a 3-column, 2-row grid
#   pdf(glue::glue("../plots/amewoo_{eval_metric}_by_elapses_nine_model_selection.pdf"), width = 6*3, height = 4*2)  # Adjust PDF dimensions as needed
#   gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2)
#   dev.off()
# }
# 
# all_transitions <- do.call(rbind, all_transitions)
# averaged_all_transitions <- all_transitions |> dplyr::group_by(model_selection, eval_metric) |> dplyr::summarise(mean=mean(value)) |> dplyr::ungroup() |> as.data.frame()
# 
# wide_data <- averaged_all_transitions %>% 
#   dplyr::filter(eval_metric %in% c("win_distance", "ll_improvement")) %>%
#   tidyr::pivot_wider(names_from = eval_metric, values_from = mean)
# 
# pdf(glue::glue("../plots/wd_plot.pdf"))  # Adjust PDF dimensions as needed
# ggplot(wide_data, aes(x = win_distance, y = ll_improvement, color = model_selection)) +
#   geom_point(size = 10) +
#   labs(
#     title = "Win Distance vs LL Improvement by Model Selection",
#     x = "Win Distance",
#     y = "LL Improvement",
#     color = "Model Selection"
#   ) +
#   scale_color_brewer(palette = "Set1") +
#   theme_minimal()
# dev.off()
# 
# 
# ###
# # Plot routes
# plot_list <- list()
# for (sp in c('amewoo', 'buwtea', 'swahaw', 'brwhaw', 'woothr')){
#   all_metrics <- load_eval_metrics(sp)
#   best_by_ll_improvement_tracking <- all_metrics[
#     order(-all_metrics$train_ll_improvement_tracking)
#     ,'model'][1]
#   best_by_mean_win_distance_tracking <- all_metrics[
#     order(-all_metrics$train_mean_win_distance_tracking)
#     ,'model'][1]
#   best_by_multi_objective_tracking <- all_metrics[
#     order(-all_metrics$train_multi_objective_metric_tracking)
#     ,'model'][1]
#   best_by_ST_threshold_090_ll_imrpovement <- all_metrics[all_metrics$end_traverse_cor.x>0.9,][
#     order(-all_metrics[all_metrics$end_traverse_cor.x>0.9,]$train_ll_improvement_tracking)
#     ,'model'][1]
#   best_by_ST_threshold_090_mean_distance <- all_metrics[all_metrics$end_traverse_cor.x>0.9,][
#     order(-all_metrics[all_metrics$end_traverse_cor.x>0.9,]$train_mean_win_distance_tracking)
#     ,'model'][1]
#   best_by_ST_threshold_090_mean_RELC <- all_metrics[all_metrics$end_traverse_cor.x>0.9,][
#     order(-all_metrics[all_metrics$end_traverse_cor.x>0.9,]$train_mean_RELC_distance_tracking)
#     ,'model'][1]
#   best_by_ST_threshold_090_mean_ELC <- all_metrics[all_metrics$end_traverse_cor.x>0.9,][
#     order(-all_metrics[all_metrics$end_traverse_cor.x>0.9,]$train_mean_ELC_distance_tracking)
#     ,'model'][1]
#   best_by_average_param_tracking <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/americas/{sp}_100km/{sp}_2022_100km_obs1.0_ent0.001924_dist0.008177_pow0.4167.hdf5')
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ll_improvement_tracking}"))
#   tmp_bf <- bf
#   routes <- route(bf, n=10)
#   route_plot0 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by LL improvement"))
#   plot_list[[length(plot_list) + 1]] <- route_plot0
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ST_threshold_090_ll_imrpovement}"))
#   routes <- route(bf, n=10)
#   route_plot <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by 0.9 threshold end traverse and LL improvement"))
#   plot_list[[length(plot_list) + 1]] <- route_plot
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_mean_win_distance_tracking}"))
#   routes <- route(bf, n=10)
#   route_plot1 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by distance metric"))
#   plot_list[[length(plot_list) + 1]] <- route_plot1
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ST_threshold_090_mean_distance}"))
#   routes <- route(bf, n=10)
#   route_plot <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by 0.9 threshold end traverse and mean win distance"))
#   plot_list[[length(plot_list) + 1]] <- route_plot
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ST_threshold_090_mean_RELC}"))
#   routes <- route(bf, n=10)
#   route_plot <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by 0.9 threshold end traverse and mean RELC"))
#   plot_list[[length(plot_list) + 1]] <- route_plot
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ST_threshold_090_mean_ELC}"))
#   routes <- route(bf, n=10)
#   route_plot <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by 0.9 threshold end traverse and mean ELC"))
#   plot_list[[length(plot_list) + 1]] <- route_plot
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_multi_objective_tracking}"))
#   routes <- route(bf, n=10)
#   route_plot2 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by Multi-objectives"))
#   plot_list[[length(plot_list) + 1]] <- route_plot2
#   
#   bf <- import_birdflow(best_by_average_param_tracking)
#   routes <- route(bf, n=10)
#   routes <- routes$data
#   routes <- routes |> BirdFlowR::Routes(metadata = tmp_bf$metadata) |> BirdFlowR::as_BirdFlowRoutes(bf=tmp_bf)
#   route_plot3 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Averaged parameter (from the 6 sp)"))
#   plot_list[[length(plot_list) + 1]] <- route_plot3
# }
# 
# 
# pdf(glue::glue("../plots/routes_eight_model_selection_tracking.pdf"), width = 6*8, height = 4*5)  # Adjust PDF dimensions as needed
# gridExtra::grid.arrange(grobs = plot_list, ncol = 8, nrow = 5)
# dev.off()
# 
# 
# 
# ###
# plot_list <- list()
# for (sp in c('amewoo', 'buwtea', 'swahaw', 'brwhaw', 'woothr')){
#   all_metrics <- load_eval_metrics(sp)
#   best_by_ll_improvement <- all_metrics[
#     order(-all_metrics$train_ll_improvement_banding_and_motus)
#     ,'model'][1]
#   best_by_mean_win_distance <- all_metrics[
#     order(-all_metrics$train_mean_win_distance_banding_and_motus)
#     ,'model'][1]
#   best_by_multi_objective <- all_metrics[
#     order(-all_metrics$train_multi_objective_metric_banding_and_motus)
#     ,'model'][1]
#   best_by_ST_threshold_090_ll_imrpovement <- all_metrics[all_metrics$end_traverse_cor.x>0.9,][
#     order(-all_metrics[all_metrics$end_traverse_cor.x>0.9,]$train_ll_improvement_banding_and_motus)
#     ,'model'][1]
#   best_by_ST_threshold_090_mean_distance <- all_metrics[all_metrics$end_traverse_cor.x>0.9,][
#     order(-all_metrics[all_metrics$end_traverse_cor.x>0.9,]$train_mean_win_distance_banding_and_motus)
#     ,'model'][1]
#   best_by_average_param_tracking <- glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/americas/{sp}_100km/{sp}_2022_100km_obs1.0_ent0.001924_dist0.008177_pow0.4167.hdf5')
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ll_improvement}"))
#   tmp_bf <- bf
#   routes <- route(bf, n=10)
#   route_plot0 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by LL improvement"))
#   plot_list[[length(plot_list) + 1]] <- route_plot0
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ST_threshold_090_ll_imrpovement}"))
#   routes <- route(bf, n=10)
#   route_plot <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by 0.9 threshold end traverse and LL improvement"))
#   plot_list[[length(plot_list) + 1]] <- route_plot
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_mean_win_distance}"))
#   routes <- route(bf, n=10)
#   route_plot1 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by distance metric"))
#   plot_list[[length(plot_list) + 1]] <- route_plot1
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_ST_threshold_090_mean_distance}"))
#   routes <- route(bf, n=10)
#   route_plot <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by 0.9 threshold end traverse and mean win distance"))
#   plot_list[[length(plot_list) + 1]] <- route_plot
#   
#   bf <- import_birdflow(glue::glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{best_by_multi_objective}"))
#   routes <- route(bf, n=10)
#   route_plot2 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Model selection by Multi-objectives"))
#   plot_list[[length(plot_list) + 1]] <- route_plot2
#   
#   bf <- import_birdflow(best_by_average_param_tracking)
#   routes <- route(bf, n=10)
#   routes <- routes$data
#   routes <- routes |> BirdFlowR::Routes(metadata = tmp_bf$metadata) |> BirdFlowR::as_BirdFlowRoutes(bf=tmp_bf)
#   route_plot3 <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = glue::glue("{sp} Averaged parameter (from the 6 sp)"))
#   plot_list[[length(plot_list) + 1]] <- route_plot3
# }
# 
# 
# # Arrange the plots in a 3-column, 2-row grid
# pdf(glue::glue("../plots/routes_two_model_selection_banding_and_motus.pdf"), width = 6*4, height = 4*5)  # Adjust PDF dimensions as needed
# gridExtra::grid.arrange(grobs = plot_list, ncol = 4,  nrow = 5)
# dev.off()
# 
# 
# ###
# 
# 
# 
# ###
# library(viridis)
# 
# # Against ll_improvement
# ggplot() + geom_point(data=each_transition_tracking[(each_transition_tracking$ll_improvement>=-5)&(each_transition_tracking$ll_improvement<=10),], 
#                       aes(x=elapsed_days, y=elapsed_km, color=ll_improvement, fill = ll_improvement), color = "black", size=3, shape = 21, stroke = 0.1, alpha=0.8)+
#   labs(title='amewoo tracking data transition pairs') + 
#   scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, oob = scales::squish, space = "Lab", limits = c(-3, 3))
# 
# ggplot() + geom_point(data=each_transition_banding[(each_transition_banding$ll_improvement>=-5)&(each_transition_banding$ll_improvement<=10),], 
#                       aes(x=elapsed_days, y=elapsed_km, color=ll_improvement, fill = ll_improvement), color = "black", size=3, shape = 21, stroke = 0.1, alpha=0.8)+
#   labs(title='amewoo banding data transition pairs') + 
#   scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, oob = scales::squish, space = "Lab", limits = c(-3, 3))
# 
# ggplot() + geom_point(data=each_transition_motus[(each_transition_motus$ll_improvement>=-5)&(each_transition_motus$ll_improvement<=10),], 
#                       aes(x=elapsed_days, y=elapsed_km, color=ll_improvement, fill = ll_improvement), color = "black", size=3, shape = 21, stroke = 0.1, alpha=0.8)+
#   labs(title='amewoo MOTUS data transition pairs') + 
#   scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, oob = scales::squish, space = "Lab", limits = c(-3, 3))
# 
# # Against win_distance_fraction
# ggplot() + geom_point(data=each_transition_tracking[(each_transition_tracking$win_distance_fraction>=-5)&(each_transition_tracking$win_distance_fraction<=10),], 
#                       aes(x=elapsed_days, y=elapsed_km, color=win_distance_fraction, fill = win_distance_fraction), color = "black", size=3, shape = 21, stroke = 0.1, alpha=0.8)+
#   labs(title='amewoo tracking data transition pairs') + 
#   scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, oob = scales::squish, space = "Lab", limits = c(-0.5, 1))
# 
# ggplot() + geom_point(data=each_transition_banding[(each_transition_banding$win_distance_fraction>=-5)&(each_transition_banding$win_distance_fraction<=10),], 
#                       aes(x=elapsed_days, y=elapsed_km, color=win_distance_fraction, fill = win_distance_fraction), color = "black", size=3, shape = 21, stroke = 0.1, alpha=0.8)+
#   labs(title='amewoo banding data transition pairs') + 
#   scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, oob = scales::squish, space = "Lab", limits = c(-0.5, 1))
# 
# ggplot() + geom_point(data=each_transition_motus[(each_transition_motus$win_distance_fraction>=-5)&(each_transition_motus$win_distance_fraction<=10),], 
#                       aes(x=elapsed_days, y=elapsed_km, color=win_distance_fraction, fill = win_distance_fraction), color = "black", size=3, shape = 21, stroke = 0.1, alpha=0.8)+
#   labs(title='amewoo MOTUS data transition pairs') + 
#   scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, oob = scales::squish, space = "Lab", limits = c(-0.5, 1))
# 
# 
# 
# library(ggplot2)
# sp <- 'amewoo'
# bf = BirdFlowR::import_birdflow('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/amewoo_2022_150km_obs1.0_ent0.002331_dist0.02331_pow0.9.hdf5')
# st_dists <- get_distr(bf, which = "all", from_marginals = FALSE)
# gcd <- great_circle_distances(bf)
# ###
# params <- set_pipeline_params(species = sp)
# params <- preprocess_species_wrapper(params)
# 
# track_birdflowroutes_obj <- get_real_track(bf, params) # Real track
# track_interval_obj <- track_birdflowroutes_obj |> 
#   BirdFlowR::as_BirdFlowIntervals(max_n=5000,
#                                   min_day_interval=1,
#                                   max_day_interval=180,
#                                   min_km_interval=0,
#                                   max_km_interval=8000)
# 
# banding_df <- load_banding_transitions_df(file.path(BirdFlowPipeline:::the$banding_rds_path, paste0(params$species, '.rds')))
# banding_birdflowroutes_obj <- banding_df |> 
#   BirdFlowR::Routes(species=bf$species, metadata=params$metadata, source=NULL) |>
#   BirdFlowR::as_BirdFlowRoutes(bf=bf)
# banding_interval_obj <-  banding_birdflowroutes_obj |>
#   BirdFlowR::as_BirdFlowIntervals(max_n=10000,
#                                   min_day_interval=1,
#                                   max_day_interval=180,
#                                   min_km_interval=0,
#                                   max_km_interval=8000)
# 
# motus_df <- load_motus_transitions_df(file.path(BirdFlowPipeline:::the$motus_rds_path, paste0(params$species, '.rds')))
# motus_df <- na.omit(motus_df)
# motus_birdflowroutes_obj <- motus_df |> 
#   BirdFlowR::Routes(species=bf$species, metadata=params$metadata, source=NULL) |>
#   BirdFlowR::as_BirdFlowRoutes(bf=bf)
# motus_interval_obj <-  motus_birdflowroutes_obj |>
#   BirdFlowR::as_BirdFlowIntervals(max_n=10000,
#                                   min_day_interval=1,
#                                   max_day_interval=180,
#                                   min_km_interval=0,
#                                   max_km_interval=8000)
# 
# # Assuming your data is stored in banding_interval_obj$data
# tmp_banding_interval_obj_data <- banding_interval_obj$data
# tmp_banding_interval_obj_data$random_y <- rnorm(nrow(banding_interval_obj$data), mean=0, sd=1)
# special_df <- tmp_banding_interval_obj_data[tmp_banding_interval_obj_data$timestep1 > tmp_banding_interval_obj_data$timestep2,]
# tmp_banding_interval_obj_data <- tmp_banding_interval_obj_data[tmp_banding_interval_obj_data$timestep1 < tmp_banding_interval_obj_data$timestep2,]
# if (nrow(special_df)!=0){
#   for (i in 1:nrow(special_df)){
#     tmp1 <- tmp2 <- special_df[i,]
#     tmp1$timestep2 <- 52
#     tmp2$timestep1 <- 1
#     tmp_banding_interval_obj_data <- rbind(tmp_banding_interval_obj_data, tmp1)
#     tmp_banding_interval_obj_data <- rbind(tmp_banding_interval_obj_data, tmp2)
#   }
# }
# 
# tmp_track_interval_obj_data <- track_interval_obj$data
# tmp_track_interval_obj_data$random_y <- rnorm(nrow(track_interval_obj$data), mean=5, sd=1)
# special_df <- tmp_track_interval_obj_data[tmp_track_interval_obj_data$timestep1 > tmp_track_interval_obj_data$timestep2,]
# tmp_track_interval_obj_data <- tmp_track_interval_obj_data[tmp_track_interval_obj_data$timestep1 < tmp_track_interval_obj_data$timestep2,]
# if (nrow(special_df)!=0){
#   for (i in 1:nrow(special_df)){
#     tmp1 <- tmp2 <- special_df[i,]
#     tmp1$timestep2 <- 52
#     tmp2$timestep1 <- 1
#     tmp_track_interval_obj_data <- rbind(tmp_track_interval_obj_data, tmp1)
#     tmp_track_interval_obj_data <- rbind(tmp_track_interval_obj_data, tmp2)
#   }
# }
# 
# tmp_motus_interval_obj_data <- motus_interval_obj$data
# tmp_motus_interval_obj_data$random_y <- rnorm(nrow(motus_interval_obj$data), mean=10, sd=1)
# special_df <- tmp_motus_interval_obj_data[tmp_motus_interval_obj_data$timestep1 > tmp_motus_interval_obj_data$timestep2,]
# tmp_motus_interval_obj_data <- tmp_motus_interval_obj_data[tmp_motus_interval_obj_data$timestep1 < tmp_motus_interval_obj_data$timestep2,]
# if (nrow(special_df)!=0){
#   for (i in 1:nrow(special_df)){
#     tmp1 <- tmp2 <- special_df[i,]
#     tmp1$timestep2 <- 52
#     tmp2$timestep1 <- 1
#     tmp_motus_interval_obj_data <- rbind(tmp_motus_interval_obj_data, tmp1)
#     tmp_motus_interval_obj_data <- rbind(tmp_motus_interval_obj_data, tmp2)
#   }
# }
# 
# ggplot() +
#   geom_segment(data = tmp_banding_interval_obj_data,
#                aes(x = timestep1, xend = timestep2, y = random_y, yend = random_y, color = "Banding"), size = 0.5, alpha=0.1) +
#   geom_segment(data = tmp_track_interval_obj_data,
#                aes(x = timestep1, xend = timestep2, y = random_y, yend = random_y, color = "Tracking"), size = 0.5, alpha=0.1) +
#   geom_segment(data = tmp_motus_interval_obj_data,
#                aes(x = timestep1, xend = timestep2, y = random_y, yend = random_y, color = "MOTUS"), size = 0.8, alpha=0.1) +
#   labs(x = "Time Step", y = "random y", color = "test", title='amewoo transition data distribution') +
#   scale_color_manual(values = c("MOTUS" = "darkgreen", "Tracking" = "red", "Banding" = "blue")) +
#   guides(color = guide_legend(override.aes = list(alpha = 1)))
# 
# 
# 
# 
# 
# 
# devtools::install_local("/home/yc85_illinois_edu/BirdFlowR", force = T, dependencies = FALSE) # if the BirdFlowR is updated, we need to reinstall it, so that i can be used in BirdFlowPipeline!
# load_all("/home/yc85_illinois_edu/BirdFlowR") 
# load_all("/home/yc85_illinois_edu/BirdFlowPipeline") 
# 
# ppath <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km_2025-02-27/each_transition_evaluation'
# each_transition_evaluation_files <- list.files(ppath)
# 
# for (file in each_transition_evaluation_files[1:10]){
#   transition_evaluation <- readRDS(file.path(ppath, file))
#   plot(transition_evaluation$elapsed_days, transition_evaluation$ll - transition_evaluation$null_ll)
#   
# }
# 
# 
# 
# 
# #####
# library(ggplot2)
# ppath <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/amewoo/amewoo_150km_2025-02-27/each_transition_evaluation/'
# all_test_transition_evaluation_files <- list.files(ppath)
# tracking_files <- grep("_tracking_", all_test_transition_evaluation_files, value = TRUE)
# plot_list <- list()
# plot_count <- 1
# for (file in tracking_files){
#   eval_data <- readRDS(file.path(ppath, file))
#   eval_data$ll_improvement <- eval_data$ll - eval_data$null_ll
#   p <- ggplot(eval_data, aes(x = elapsed_days, y = ll_improvement)) +
#     geom_point(alpha=0.5) +
#     geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
#     geom_smooth(method = "loess", se = FALSE, color = "blue") + 
#     ggtitle(file) +
#     theme(plot.title = element_text(size = 10))
#   plot_list[[plot_count]] <- p
#   plot_count <- plot_count+1
# }
# 
# 
# pdf(glue::glue("../plots/all_225_model_for_amewoo.pdf"), width = 6*9, height = 4*25)  # Adjust PDF dimensions as needed
# gridExtra::grid.arrange(grobs = plot_list, ncol = 9)  # Adjust 'ncol' to control the number of plots per row
# dev.off()
# 
# 
# 
# ####### plot prediction projection
# sp <- 'amewoo'
# bf <- BirdFlowR::import_birdflow('/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/americas/amewoo_100km/amewoo_2022_100km_obs1.0_ent0.001924_dist0.008177_pow0.4167.hdf5')
# st_dists <- get_distr(bf, which = "all", from_marginals = FALSE)
# gcd <- great_circle_distances(bf)
# 
# ###
# params <- set_pipeline_params(species = sp)
# params <- preprocess_species_wrapper(params)
# 
# track_birdflowroutes_obj <- get_real_track(bf, params) # Real track
# track_interval_obj <- track_birdflowroutes_obj |> 
#   BirdFlowR::as_BirdFlowIntervals(max_n=5000,
#                                   min_day_interval=7,
#                                   max_day_interval=180,
#                                   min_km_interval=15,
#                                   max_km_interval=8000)
# 
# # # # Calculate distance metric & ll
# each_transition_tracking <- read.csv('/home/yc85_illinois_edu/each_transition_tracking.csv')
# # plot(x=each_transition_tracking$win_distance_fraction, y=each_transition_tracking$ll_improvement)
# plot(x = each_transition_tracking$win_distance_fraction, 
#      y = each_transition_tracking$ll_improvement, 
#      col = ifelse(each_transition_tracking$i1 == each_transition_tracking$i2, "red", "black"),
#      pch = 16)
# abline(h=0, col = "red", lwd = 2, lty = 2)
# abline(v=0, col = "red", lwd = 2, lty = 2)
# 
# 
# # each_transition_banding <- read.csv('/home/yc85_illinois_edu/each_transition_banding.csv')
# # plot(x=each_transition_banding$win_distance_fraction, y=each_transition_banding$ll_improvement)
# # abline(h=0, col = "red", lwd = 2, lty = 2)
# # abline(v=0, col = "red", lwd = 2, lty = 2)
# 
# # tmp <- each_transition_tracking[order(-each_transition_tracking$win_distance_fraction),]
# tmp <- each_transition_tracking[(each_transition_tracking$win_distance_fraction>=0.2)&(each_transition_tracking$win_distance_fraction<=0.8)&(each_transition_tracking$ll_improvement<0),]
# tmp <- tmp[tmp$win_distance_fraction!=1,]
# tmp <- tmp[tmp$elapsed_days<=30,]
# tmp <- tmp[tmp$i1!=tmp$i2]
# tmp <- tmp[1:20, ]
# # tmp_track_interval_obj_data <- track_interval_obj$data[track_interval_obj$data$route_id %in% tmp$route_id,]
# splitted_track_interval <- split(tmp, seq(nrow(tmp)))
# 
# library(ggplot2)
# plot_list <- list()
# plot_count <- 1
# 
# for (name_ in names(splitted_track_interval)){
#   birdflow_interval_row <- splitted_track_interval[[name_]]
#     
#   # latlong data for banding and encounter location
#   point_df_initial <- data.frame(x = birdflow_interval_row$lon1, y = birdflow_interval_row$lat1)
#   point_df_final   <- data.frame(x = birdflow_interval_row$lon2, y = birdflow_interval_row$lat2)
#   # birdflow one-hot distributions for banding and encounter locations
#   d_initial <- as_distr(x = point_df_initial, bf = bf, crs = 'EPSG:4326') # same as birdflow_interval_row$i1
#   d_final <- as_distr(x = point_df_final, bf = bf, crs = 'EPSG:4326') # same as birdflow_interval_row$i2
#   # get s&t distribution for final timestep
#   final_timestep <- birdflow_interval_row$timestep2
#   final_st_distr <- st_dists[,final_timestep]
#   # birdflow cell index for encounter location
#   i_final <- which(d_final == 1)
#   # birdflow predictions from banding one-hot, for encounter date
#   preds <- predict(bf, d_initial, start = birdflow_interval_row$date1, end = birdflow_interval_row$date2)
#   preds_final <- preds[,ncol(preds),drop = FALSE]
#   preds_final <- as.vector(preds_final)
# 
#   xy <- i_to_xy(seq_along(preds[,ncol(preds)]), bf)
#   xy$pred_p <- preds_final
#   xy$st_p <- final_st_distr
#   
#   xy_start_end <- i_to_xy(c(which(d_initial==1), which(d_final==1)), bf)
#   xy_start_end <- as.data.frame(list(
#     x=xy_start_end$x[1],
#     y=xy_start_end$y[1],
#     xend=xy_start_end$x[2],
#     yend=xy_start_end$y[2]
#   ))
# 
#   # common_min <- min(c(xy$pred_p, xy$st_p), na.rm = TRUE)
#   # common_max <- max(c(xy$pred_p, xy$st_p), na.rm = TRUE)
#   # common_scale <- scale_fill_viridis_c(name = "Probability", 
#   #                                      limits = c(common_min, common_max))
#   
#   p1 <- ggplot() + 
#     geom_tile(data = xy, aes(x = x, y = y, fill = pred_p)) +
#     geom_segment(data = xy_start_end, 
#                  aes(x = x, y = y, xend = xend, yend = yend), 
#                  arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
#                  linewidth = 1, color = 'red', alpha = 0.8) +
#     scale_fill_viridis_c(name = "Probability") +
#     theme_minimal()
#   
#   p2 <- ggplot() + 
#     geom_tile(data = xy, aes(x = x, y = y, fill = st_p)) +
#     geom_segment(data = xy_start_end, 
#                  aes(x = x, y = y, xend = xend, yend = yend), 
#                  arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
#                  linewidth = 1, color = 'red', alpha = 0.8) +
#     scale_fill_viridis_c(name = "Probability") +
#     theme_minimal()
#   
#   ll <- log(preds_final[i_final]+1e-8)
#   ll_null <- log(final_st_distr[i_final]+1e-8)
#   win_distance_fraction <- birdflow_interval_row$win_distance_fraction
#   title = glue::glue("LL: {round(ll, 2)} (left);  Null LL: {round(ll_null, 2)} (right); win distnace fraction: {round(win_distance_fraction, 2)}")
#   
#   combined_plot <- p1 + p2 +
#     patchwork::plot_annotation(title = title)
#   
#   plot_list[[plot_count]] <- combined_plot
#   plot_count <- plot_count + 1
# }
# 
# plot_list_grob <- lapply(plot_list, patchwork::patchworkGrob)
# pdf(glue::glue("../plots/plot_each_tansition_prediction_amewoo.pdf"), width = 5*2, height = 4*length(plot_list))  # Adjust PDF dimensions as needed
# gridExtra::grid.arrange(grobs = plot_list_grob, ncol = 1)  # Adjust 'ncol' to control the number of plots per row
# dev.off()
# 
# 
# 
# 
# 
# 
# # 
# # ## What is the influence of shifting the rank function?
# # library(ggplot2)
# # library(dplyr)
# # library(glue)
# # library(gridExtra)
# # 
# # # different rank model methods:
# # # load params
# # # load eval_metrics
# # get_latest_metrics_file <- function(sp_output_path){
# #   all_files <- list.files(
# #     path = sp_output_path,
# #     pattern = "^eval_metrics\\.rds$",
# #     full.names = TRUE,
# #     recursive = TRUE
# #   )
# #   file_info <- file.info(all_files)
# #   file_info$file_path <- rownames(file_info)
# #   latest_file <- file_info[which.max(file_info$mtime), "file_path"]
# #   return(latest_file)
# # }
# # 
# # plot_ranking_differences <- function(metrics_by_distance, metrics_by_tracking, sp){
# #   merged_metrics <- merge(metrics_by_distance[,c('model','overall_des')],
# #                           metrics_by_tracking[,c('model','overall_des')], by='model',
# #                           suffixes = c("_distance", "_tracking"))
# #   merged_metrics <- merged_metrics[order(-merged_metrics$overall_des_distance), ]
# #   merged_metrics$point_color <- ifelse(merged_metrics$model==head(merged_metrics, 1)$model, 'red', 'black')
# #   merged_metrics$point_alpha <- ifelse(merged_metrics$model == head(merged_metrics, 1)$model, 1, 0.8)
# #   merged_metrics$point_size <- ifelse(merged_metrics$model == head(merged_metrics, 1)$model, 0.7, 0.6)
# #   merged_metrics$point_color <- factor(merged_metrics$point_color)
# #   
# #   p <- ggplot(data=merged_metrics) +
# #     geom_point(aes(x=overall_des_tracking, y=overall_des_distance, color=point_color, alpha = point_alpha, size = point_size)) +
# #     scale_color_manual(values = c("red" = "red", "black" = "black")) +
# #     scale_size_continuous(range = c(1, 3), guide = "none") +
# #     scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
# #     theme_minimal() +
# #     ggtitle(sp) + 
# #     theme(legend.position = "none")
# #   
# #   return(p)
# # }
# # 
# # plot_ranking_differences_sub_objectives <- function(metrics_by_distance, sub_objective, sp){
# #   merged_metrics <- metrics_by_distance
# #   merged_metrics <- merged_metrics[order(-merged_metrics$overall_de), ]
# #   merged_metrics$point_color <- ifelse(merged_metrics$model==head(merged_metrics, 1)$model, 'red', 'black')
# #   merged_metrics$point_alpha <- ifelse(merged_metrics$model == head(merged_metrics, 1)$model, 1, 0.8)
# #   merged_metrics$point_size <- ifelse(merged_metrics$model == head(merged_metrics, 1)$model, 0.7, 0.6)
# #   merged_metrics$point_color <- factor(merged_metrics$point_color)
# #   
# #   p <- ggplot(data=merged_metrics) +
# #     geom_point(aes(x=.data[[sub_objective]], y=win_prob_d, color=point_color, alpha = point_alpha, size = point_size)) +
# #     scale_color_manual(values = c("red" = "red", "black" = "black")) +
# #     scale_size_continuous(range = c(1, 3), guide = "none") +
# #     scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
# #     theme_minimal() +
# #     ggtitle(sp) + 
# #     theme(legend.position = "none")
# #   
# #   return(p)
# # }
# # 
# # 
# # plot_list_metrics <- list()
# # plot_list_sub_objectives <- list()
# # plot_list_routes <- list()
# # for (sp in species_list){
# #   sp_output_path <- paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric','/',sp)
# #   metrics_file <- get_latest_metrics_file(sp_output_path)
# #   metrics <- readRDS(metrics_file)
# #   
# #   # param1: model_selection = 'distance_metric'
# #   params <- list()
# #   params$model_selection <- 'distance_metric'
# #   metrics_by_distance <- rank_models(metrics, params)
# #   metrics_by_distance$ranking <- seq_along(rownames(metrics_by_distance))
# #   
# #   # param2: model_selection = 'real_tracking'
# #   # params <- list()
# #   params <- set_pipeline_params(species = sp, hdf_path = sp_output_path,
# #                                 base_output_path = sp_output_path,
# #                                 model_selection = 'real_tracking')
# #   params <- preprocess_species_wrapper(params)
# #   metrics_by_tracking <- rank_models(metrics, params)
# #   metrics_by_tracking$ranking <- seq_along(rownames(metrics_by_tracking))
# #   p <- plot_ranking_differences(metrics_by_distance, metrics_by_tracking, sp)
# #   plot_list_metrics[[sp]] <- p
# #   
# #   # Plot routes
# #   bf <- import_birdflow(glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{metrics_by_distance$model[1]}"))
# #   routes <- route(bf, n=10)
# #   p_by_distance <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = "Model selection by distance metric")
# #   plot_list_routes[[glue('{sp}_distance')]] <- p_by_distance
# #   
# #   bf <- import_birdflow(glue("/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{metrics_by_tracking$model[1]}"))
# #   routes <- route(bf, n=10)
# #   p_by_tracking <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = "Model selection by objective loss")
# #   plot_list_routes[[glue('{sp}_objective')]] <- p_by_tracking
# #   
# #   ## Which objective sub function does not align with the distance-based ranking?
# #   metrics_by_distance <- merge(metrics_by_distance, metrics_by_tracking[,c('model','pit_d', 'etc_d','str_d','nso_d')], by = 'model', all.x=TRUE)
# #   for (sub_obj in c('pit_d', 'etc_d', 'str_d', 'nso_d')){
# #     p <- plot_ranking_differences_sub_objectives(metrics_by_distance, sub_obj, sp)
# #     plot_list_sub_objectives[[glue('{sp}_{sub_obj}')]] <- p
# #   }
# #   
# #   break
# # }
# # 
# # 
# # plots_per_page <- length(species_list)  # Adjust the number of plots per page
# # pdf("../plots/plot_ranking_differences.pdf", width = 6*3, height = 4*2)  # Adjust PDF dimensions as needed
# # grid.arrange(grobs = plot_list_metrics, ncol = 3)  # Adjust 'ncol' to control the number of plots per row
# # dev.off()
# # 
# # pdf("../plots/plot_ranking_differences_sub_obj.pdf", width = 6*6, height = 4*4)  # Adjust PDF dimensions as needed
# # grid.arrange(
# #   grobs = c(plot_list_sub_objectives[seq(1, length(plot_list_sub_objectives), by = 4)],
# #             plot_list_sub_objectives[seq(2, length(plot_list_sub_objectives), by = 4)],
# #             plot_list_sub_objectives[seq(3, length(plot_list_sub_objectives), by = 4)],
# #             plot_list_sub_objectives[seq(4, length(plot_list_sub_objectives), by = 4)]), 
# #   ncol = 6,
# #   layout_matrix = rbind(seq(1, 6), seq(7, 12), seq(13, 18), seq(19, 24))
# # )
# # dev.off()
# # 
# # 
# # even_plots <- plot_list_routes[seq(2, length(plot_list_routes), by = 2)]  # Plots at even indices
# # odd_plots <- plot_list_routes[seq(1, length(plot_list_routes), by = 2)]   # Plots at odd indices
# # 
# # pdf("../plots/plot_routes_differences_by_different_model_selection_methods.pdf", width = 6*6, height = 4*2)  # Adjust PDF dimensions as needed
# # grid.arrange(
# #   grobs = c(odd_plots, even_plots), ncol = 6,
# #   layout_matrix = rbind(seq(1, 6), seq(7, 12))
# #   )
# # dev.off()
# # 
# # 
# # 
# # ###
# # track_info <- make_tracks(file.path(BirdFlowPipeline:::the$banding_rds_path, paste0(params$species, '.rds')))
# # 
# # as.data.frame(track_info$int_df)
# # 
# # # 
# # # #####
# # # species_list <- c('amewoo') #, 'buwtea', 'lobcur', 'swahaw', 'brwhaw', 'woothr'
# # # 
# # # for (sp in species_list){
# # #   sp_output_path <- paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams','/',sp)
# # #   if (!dir.exists(sp_output_path)){dir.create(sp_output_path, recursive = TRUE)}
# # #   
# # #   tryCatch({
# # #     batch_flow(sp, 
# # #                gpu_ram = 10,
# # #                hdf_path = sp_output_path,
# # #                base_output_path = sp_output_path,
# # #                model_selection = 'real_tracking')
# # #   }, error = function(e) {
# # #     cat("ERROR:", conditionMessage(e), "\n")
# # #   })
# # # }
# # 
# # 
# # 
# # 
