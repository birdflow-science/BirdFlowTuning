library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)


paths <- Sys.glob("/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric_log_obs/*/*_150km_interval_based_eval_using_migration_transitions")
# print(paths)

### load data
## Round 1: Load all the raw evaluation
load_raw_validation_all_sp <- function() {
  raw_combined <- list()
  raw_combined_with_tracking <- list()
  for (path in paths){
    sp <- basename(dirname(path))
    if (!(file.exists(paste0(path,'/eval_metrics_train_distance_metric_all_combined.rds')) & file.exists(paste0(path,'/eval_metrics_test_distance_metric_all_combined.rds')))){
      next
    }
    ## Load 1
    train_interval_based <- readRDS(paste0(path,'/eval_metrics_train_distance_metric_all_combined.rds'))
    if (!'end_traverse_cor_log' %in% names(train_interval_based)) {
      next
    }
    train_interval_based$weighted_mean_ll_improvement <- train_interval_based$weighted_mean_ll - train_interval_based$weighted_mean_null_ll
    train_interval_based$mean_ll_improvement <- train_interval_based$mean_ll - train_interval_based$mean_null_ll
    train_interval_based <- train_interval_based |> dplyr::filter((!is.na(.data[['pit_row']])) & (!is.na(.data[['pit_col']])) & (!is.na(.data[['pit_in_95']])))
    # train_interval_based$weighted_mean_win_distance_quantile <- (train_interval_based$weighted_mean_win_distance - min(train_interval_based$weighted_mean_win_distance))/(max(train_interval_based$weighted_mean_win_distance) - min(train_interval_based$weighted_mean_win_distance))
    # train_interval_based$weighted_mean_ll_improvement_quantile <- (train_interval_based$weighted_mean_ll_improvement - min(train_interval_based$weighted_mean_ll_improvement))/(max(train_interval_based$weighted_mean_ll_improvement) - min(train_interval_based$weighted_mean_ll_improvement))
    train_interval_based$mean_win_distance_fraction_quantile <- (train_interval_based$mean_win_distance_fraction - min(train_interval_based$mean_win_distance_fraction))/(max(train_interval_based$mean_win_distance_fraction) - min(train_interval_based$mean_win_distance_fraction))
    train_interval_based$mean_ll_improvement_quantile <- (train_interval_based$mean_ll_improvement - min(train_interval_based$mean_ll_improvement))/(max(train_interval_based$mean_ll_improvement) - min(train_interval_based$mean_ll_improvement))
    train_interval_based$ST09_threshold_LL_distance_score <- scale(train_interval_based$weighted_mean_win_distance) + scale(train_interval_based$weighted_mean_ll_improvement)
    train_interval_based$ST09_threshold_LL_distance_score <- ifelse(train_interval_based$end_traverse_cor>0.9, train_interval_based$ST09_threshold_LL_distance_score, -999)
    
    ideal <- c(max(scale(train_interval_based$end_traverse_cor)), max(scale(train_interval_based$weighted_mean_ll_improvement)))      # the utopia point
    train_interval_based$ST_and_LL <- 1 / ((scale(train_interval_based$end_traverse_cor) - ideal[1])^2 +
                                             (scale(train_interval_based$weighted_mean_ll_improvement) - ideal[2])^2 + 1e-6) # distance to the best corner
    ideal <- c(max(scale(train_interval_based$end_traverse_cor)), max(scale(train_interval_based$weighted_energy_improvement)))      # the utopia point
    train_interval_based$ST_and_energy <- 1 / ((scale(train_interval_based$end_traverse_cor) - ideal[1])^2 +
                                             (scale(train_interval_based$weighted_energy_improvement) - ideal[2])^2 + 1e-6) # distance to the best corner
    
    ideal <- c(max(scale(train_interval_based$end_traverse_cor_log)), max(scale(train_interval_based$weighted_mean_ll_improvement)))      # the utopia point
    train_interval_based$ST_and_LL_log <- 1 / ((scale(train_interval_based$end_traverse_cor_log) - ideal[1])^2 +
                                             (scale(train_interval_based$weighted_mean_ll_improvement) - ideal[2])^2 + 1e-6) # distance to the best corner
    ideal <- c(max(scale(train_interval_based$end_traverse_cor_log)), max(scale(train_interval_based$weighted_energy_improvement)))      # the utopia point
    train_interval_based$ST_and_energy_log <- 1 / ((scale(train_interval_based$end_traverse_cor_log) - ideal[1])^2 +
                                                 (scale(train_interval_based$weighted_energy_improvement) - ideal[2])^2 + 1e-6) # distance to the best corner
    
    raw_combined[[sp]] <- train_interval_based
    
    ##
    if (file.exists(paste0(path,'/eval_metrics_train_multi_objective_all_combined.rds'))){
      ## Load 3
      train_multi_objective <- readRDS(paste0(path,'/eval_metrics_train_multi_objective_all_combined.rds'))
      train_interval_based <- train_interval_based |> dplyr::select(-c('overall_des')) |> merge(train_multi_objective[,c('model',
                                                                                                                         'straightness_diff', 'n_stopovers_diff', 'speed_diff', 'overall_des')] #  'pit_d' will be from interval-based data!
                                                                                                , by = 'model')
      train_interval_based$straightness_diff_d <- (-train_interval_based$straightness_diff - min(-train_interval_based$straightness_diff))/(max(-train_interval_based$straightness_diff) - min(-train_interval_based$straightness_diff))
      train_interval_based$n_stopovers_diff_d <- (-train_interval_based$n_stopovers_diff - min(-train_interval_based$n_stopovers_diff))/(max(-train_interval_based$n_stopovers_diff) - min(-train_interval_based$n_stopovers_diff))
      train_interval_based$speed_diff_d <- (-train_interval_based$speed_diff - min(-train_interval_based$speed_diff))/(max(-train_interval_based$speed_diff) - min(-train_interval_based$speed_diff))
      raw_combined_with_tracking[[sp]] <- train_interval_based
    }
  }
  
  common_cols <- Reduce(intersect, lapply(raw_combined, names))
  raw_combined <- do.call(rbind, lapply(raw_combined, function(df) df[, common_cols, drop = FALSE]))
  raw_combined$sp <- sub("(.*)_2022_150km_.*\\.hdf5$", "\\1", raw_combined$model)
  raw_combined$model_param <- sub(".*_2022_150km_(.*)\\.hdf5$", "\\1", raw_combined$model)
  ent <- as.numeric(sub(".*_ent([0-9.]+)_dist.*", "\\1", raw_combined$model))
  dist <- as.numeric(sub(".*_dist([0-9.]+)_pow.*", "\\1", raw_combined$model))
  pow <- as.numeric(sub(".*_pow([0-9.]+)\\.hdf5", "\\1", raw_combined$model))
  raw_combined <- cbind(raw_combined, data.frame(ent, dist, pow))
  
  common_cols <- Reduce(intersect, lapply(raw_combined_with_tracking, names))
  raw_combined_with_tracking <- do.call(rbind, lapply(raw_combined_with_tracking, function(df) df[, common_cols, drop = FALSE]))
  raw_combined_with_tracking$sp <- sub("(.*)_2022_150km_.*\\.hdf5$", "\\1", raw_combined_with_tracking$model)
  raw_combined_with_tracking$model_param <- sub(".*_2022_150km_(.*)\\.hdf5$", "\\1", raw_combined_with_tracking$model)
  ent <- as.numeric(sub(".*_ent([0-9.]+)_dist.*", "\\1", raw_combined_with_tracking$model))
  dist <- as.numeric(sub(".*_dist([0-9.]+)_pow.*", "\\1", raw_combined_with_tracking$model))
  pow <- as.numeric(sub(".*_pow([0-9.]+)\\.hdf5", "\\1", raw_combined_with_tracking$model))
  raw_combined_with_tracking <- cbind(raw_combined_with_tracking, data.frame(ent, dist, pow))
  
  return(list(raw_combined=raw_combined, raw_combined_with_tracking=raw_combined_with_tracking))
}


load_best_models_validation_all_sp <- function(raw_combined, raw_combined_with_tracking, include_taxomany_LOO=FALSE) {
  ## Round 2: only record the best models
  load_interval_based_df <- function(path) {
    interval_based <- readRDS(path)
    interval_based$weighted_mean_ll_improvement <- interval_based$weighted_mean_ll - interval_based$weighted_mean_null_ll
    interval_based$mean_ll_improvement <- interval_based$mean_ll - interval_based$mean_null_ll
    interval_based <- interval_based |> dplyr::filter((!is.na(.data[['pit_row']])) & (!is.na(.data[['pit_col']])) & (!is.na(.data[['pit_in_95']])))
    if (nrow(interval_based) == 0){
      return(NULL)
    }
    
    interval_based$mean_win_distance_fraction_quantile <- (interval_based$mean_win_distance_fraction - min(interval_based$mean_win_distance_fraction))/(max(interval_based$mean_win_distance_fraction) - min(interval_based$mean_win_distance_fraction))
    interval_based$mean_ll_improvement_quantile <- (interval_based$mean_ll_improvement - min(interval_based$mean_ll_improvement))/(max(interval_based$mean_ll_improvement) - min(interval_based$mean_ll_improvement))
    interval_based$ST09_threshold_LL_distance_score <- scale(interval_based$weighted_mean_win_distance) + scale(interval_based$weighted_mean_ll_improvement)
    interval_based$ST09_threshold_LL_distance_score <- ifelse(interval_based$end_traverse_cor>0.9, interval_based$ST09_threshold_LL_distance_score, -999)
    
    ideal <- c(max(scale(interval_based$end_traverse_cor)), max(scale(interval_based$weighted_mean_ll_improvement)))      # the utopia point
    interval_based$ST_and_LL <- 1 / ((scale(interval_based$end_traverse_cor) - ideal[1])^2 +
                                             (scale(interval_based$weighted_mean_ll_improvement) - ideal[2])^2 + 1e-6) # distance to the best corner
    ideal <- c(max(scale(interval_based$end_traverse_cor)), max(scale(interval_based$weighted_energy_improvement)))      # the utopia point
    interval_based$ST_and_energy <- 1 / ((scale(interval_based$end_traverse_cor) - ideal[1])^2 +
                                                 (scale(interval_based$weighted_energy_improvement) - ideal[2])^2 + 1e-6) # distance to the best corner
    
    ideal <- c(max(scale(interval_based$end_traverse_cor_log)), max(scale(interval_based$weighted_mean_ll_improvement)))      # the utopia point
    interval_based$ST_and_LL_log <- 1 / ((scale(interval_based$end_traverse_cor_log) - ideal[1])^2 +
                                       (scale(interval_based$weighted_mean_ll_improvement) - ideal[2])^2 + 1e-6) # distance to the best corner
    ideal <- c(max(scale(interval_based$end_traverse_cor_log)), max(scale(interval_based$weighted_energy_improvement)))      # the utopia point
    interval_based$ST_and_energy_log <- 1 / ((scale(interval_based$end_traverse_cor_log) - ideal[1])^2 +
                                           (scale(interval_based$weighted_energy_improvement) - ideal[2])^2 + 1e-6) # distance to the best corner
    
    interval_based <- interval_based |>
      dplyr::mutate(
        d_pit_row = desirability2::d_min(.data$pit_row, use_data = TRUE),
        d_pit_col = desirability2::d_min(.data$pit_col, use_data = TRUE),
        d_pit_in_95 = ifelse(
          diff(range(abs(.data$pit_in_95 - 0.95))) == 0,
          1,
          desirability2::d_min(abs(.data$pit_in_95 - 0.95), use_data = TRUE)
        ),
        pit_d = desirability2::d_overall(dplyr::across(dplyr::starts_with("d_pit"))),
      )
    return(interval_based)
  }
  

  trait_data <- read.csv('../../data/00.sp_info/All_combined_eco_function_traits.csv')
  trait_data <- ebirdst::ebirdst_runs[,c('species_code', 'common_name')] |> merge(trait_data, by.x='common_name', by.y='Common_Name1_eBird', all.x=T)
  raw_combined <- raw_combined |> merge(trait_data, by.x= 'sp', by.y='species_code', all.x=T)
  raw_combined_with_tracking <- raw_combined_with_tracking |> merge(trait_data, by.x= 'sp', by.y='species_code', all.x=T)

  all_res <- list()
  all_res_with_tracking <- list()
  for (path in paths){
    sp <- basename(dirname(path))
    family_ <- c(trait_data[trait_data$species_code==sp, 'FAMILY1_eBird'])[1]
    order_ <- c(trait_data[trait_data$species_code==sp, 'ORDER1_eBird'])[1]
  
    if (!(file.exists(paste0(path,'/eval_metrics_train_distance_metric_all_combined.rds')) & file.exists(paste0(path,'/eval_metrics_test_distance_metric_all_combined.rds')))){
      next
    }
    ## Load 1
    train_interval_based <- load_interval_based_df(path=paste0(path,'/eval_metrics_train_distance_metric_all_combined.rds'))
    if (is.null(train_interval_based)){
      next
    }
    # 
    if (!('synth_routes_prebreeding_migration_straightness' %in% names(train_interval_based))){
      next
    }
    
    ## Load 2
    test_interval_based <- load_interval_based_df(path=paste0(path,'/eval_metrics_test_distance_metric_all_combined.rds'))
    if (is.null(test_interval_based)){
      next
    }
    
    ## Get biological metrics here!!
    if (file.exists(paste0(path,'/eval_metrics_train_multi_objective_all_combined.rds'))){
      ## Load 3
      train_multi_objective <- readRDS(paste0(path,'/eval_metrics_train_multi_objective_all_combined.rds'))
      train_interval_based <- train_interval_based |> merge(train_multi_objective[,c('model',
                                                                                     'straightness_diff', 'n_stopovers_diff', 'speed_diff')] #  'pit_d' will be from interval-based data!
                                                            , by = 'model')
      train_interval_based$straightness_diff_d <- (-train_interval_based$straightness_diff - min(-train_interval_based$straightness_diff))/(max(-train_interval_based$straightness_diff) - min(-train_interval_based$straightness_diff))
      train_interval_based$n_stopovers_diff_d <- (-train_interval_based$n_stopovers_diff - min(-train_interval_based$n_stopovers_diff))/(max(-train_interval_based$n_stopovers_diff) - min(-train_interval_based$n_stopovers_diff))
      train_interval_based$speed_diff_d <- (-train_interval_based$speed_diff - min(-train_interval_based$speed_diff))/(max(-train_interval_based$speed_diff) - min(-train_interval_based$speed_diff))
    }
    
    # Best models
    best_model_by_ST09_threshold_LL_distance_score <- (train_interval_based |> dplyr::arrange(-ST09_threshold_LL_distance_score))$model[1]
    best_model_by_ST_and_LL <- (train_interval_based |> dplyr::arrange(-ST_and_LL))$model[1]
    best_model_by_ST_and_energy <- (train_interval_based |> dplyr::arrange(-ST_and_energy))$model[1]
    best_model_by_ST_and_LL_log <- (train_interval_based |> dplyr::arrange(-ST_and_LL_log))$model[1]
    best_model_by_ST_and_energy_log <- (train_interval_based |> dplyr::arrange(-ST_and_energy_log))$model[1]
    best_model_by_LL <- (train_interval_based |> dplyr::arrange(-weighted_mean_ll_improvement))$model[1]
    best_model_by_distance_metric <- (train_interval_based |> dplyr::arrange(-weighted_mean_win_distance))$model[1]
    best_model_by_energy_score <- (train_interval_based |> dplyr::arrange(-weighted_energy_improvement))$model[1]
    best_model_by_energy_score_days_integral <- (train_interval_based |> dplyr::arrange(-weighted_energy_improvement_days_integral))$model[1]
    best_param_by_LOO_energy_score <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                    dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                    dplyr::group_by(.data[['model_param']]) |>
                                                                    dplyr::summarize(
                                                                      weighted_energy_improvement = mean(.data[['weighted_energy_improvement']])
                                                                    ) |>
                                                                    dplyr::arrange(-.data[['weighted_energy_improvement']]) |>
                                                                    dplyr::ungroup()
    )$model_param[1], '.hdf5')
    
    best_param_by_LOO_ST09_threshold_LL_distance_score <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                        dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                        dplyr::group_by(.data[['model_param']]) |>
                                                                                        dplyr::summarize(
                                                                                          ST09_threshold_LL_distance_score = mean(.data[['ST09_threshold_LL_distance_score']])
                                                                                        ) |>
                                                                                        dplyr::arrange(-.data[['ST09_threshold_LL_distance_score']]) |>
                                                                                        dplyr::ungroup()
    )$model_param[1], '.hdf5')
    best_param_by_LOO_ST_and_LL <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                        dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                        dplyr::group_by(.data[['model_param']]) |>
                                                                                        dplyr::summarize(
                                                                                          ST_and_LL = mean(.data[['ST_and_LL']])
                                                                                        ) |>
                                                                                        dplyr::arrange(-.data[['ST_and_LL']]) |>
                                                                                        dplyr::ungroup()
    )$model_param[1], '.hdf5')
    best_param_by_LOO_ST_and_energy <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                 dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                 dplyr::group_by(.data[['model_param']]) |>
                                                                 dplyr::summarize(
                                                                   ST_and_energy = mean(.data[['ST_and_energy']])
                                                                 ) |>
                                                                 dplyr::arrange(-.data[['ST_and_energy']]) |>
                                                                 dplyr::ungroup()
    )$model_param[1], '.hdf5')
    best_param_by_LOO_ST_and_LL_log <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                 dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                 dplyr::group_by(.data[['model_param']]) |>
                                                                 dplyr::summarize(
                                                                   ST_and_LL_log = mean(.data[['ST_and_LL_log']])
                                                                 ) |>
                                                                 dplyr::arrange(-.data[['ST_and_LL_log']]) |>
                                                                 dplyr::ungroup()
    )$model_param[1], '.hdf5')
    best_param_by_LOO_ST_and_energy_log <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                     dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                     dplyr::group_by(.data[['model_param']]) |>
                                                                     dplyr::summarize(
                                                                       ST_and_energy_log = mean(.data[['ST_and_energy_log']])
                                                                     ) |>
                                                                     dplyr::arrange(-.data[['ST_and_energy_log']]) |>
                                                                     dplyr::ungroup()
    )$model_param[1], '.hdf5')
    best_param_by_LOO_LL <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                          dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                          dplyr::group_by(.data[['model_param']]) |>
                                                          dplyr::summarize(
                                                            weighted_mean_ll = mean(.data[['weighted_mean_ll']])
                                                          ) |>
                                                          dplyr::arrange(-.data[['weighted_mean_ll']]) |>
                                                          dplyr::ungroup()
    )$model_param[1], '.hdf5')
    best_param_by_LOO_distance_metric <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                       dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                       dplyr::group_by(.data[['model_param']]) |>
                                                                       dplyr::summarize(
                                                                         weighted_mean_win_distance = mean(.data[['weighted_mean_win_distance']])
                                                                       ) |>
                                                                       dplyr::arrange(-.data[['weighted_mean_win_distance']]) |>
                                                                       dplyr::ungroup()
    )$model_param[1], '.hdf5')
    best_param_by_daves_multiobjective <- paste0(sp, '_2022_150km_', (raw_combined_with_tracking |> 
                                                                        dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                        dplyr::group_by(.data[['model_param']]) |>
                                                                        dplyr::summarize(
                                                                          overall_des = mean(.data[['overall_des']])
                                                                        ) |>
                                                                        dplyr::arrange(-.data[['overall_des']]) |>
                                                                        dplyr::ungroup())$model_param[1], '.hdf5')
    
    if (include_taxomany_LOO) {
      best_param_by_LOO_energy_score_FAMILY <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                      dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                      dplyr::filter(.data[['FAMILY1_eBird']] == {{family_}}) |>
                                                                      dplyr::group_by(.data[['model_param']]) |>
                                                                      dplyr::summarize(
                                                                        weighted_energy_improvement = mean(.data[['weighted_energy_improvement']])
                                                                      ) |>
                                                                      dplyr::arrange(-.data[['weighted_energy_improvement']]) |>
                                                                      dplyr::ungroup()
      )$model_param[1], '.hdf5')
      
      best_param_by_LOO_ST09_threshold_LL_distance_score_FAMILY <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                          dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                          dplyr::filter(.data[['FAMILY1_eBird']] == {{family_}}) |>
                                                                                          dplyr::group_by(.data[['model_param']]) |>
                                                                                          dplyr::summarize(
                                                                                            ST09_threshold_LL_distance_score = mean(.data[['ST09_threshold_LL_distance_score']])
                                                                                          ) |>
                                                                                          dplyr::arrange(-.data[['ST09_threshold_LL_distance_score']]) |>
                                                                                          dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_ST09_threshold_ST_and_LL_FAMILY <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                                 dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                                 dplyr::filter(.data[['FAMILY1_eBird']] == {{family_}}) |>
                                                                                                 dplyr::group_by(.data[['model_param']]) |>
                                                                                                 dplyr::summarize(
                                                                                                   ST_and_LL = mean(.data[['ST_and_LL']])
                                                                                                 ) |>
                                                                                                 dplyr::arrange(-.data[['ST_and_LL']]) |>
                                                                                                 dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_ST09_threshold_ST_and_energy_FAMILY <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                         dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                         dplyr::filter(.data[['FAMILY1_eBird']] == {{family_}}) |>
                                                                                         dplyr::group_by(.data[['model_param']]) |>
                                                                                         dplyr::summarize(
                                                                                           ST_and_energy = mean(.data[['ST_and_energy']])
                                                                                         ) |>
                                                                                         dplyr::arrange(-.data[['ST_and_energy']]) |>
                                                                                         dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_ST09_threshold_ST_and_LL_log_FAMILY <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                         dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                         dplyr::filter(.data[['FAMILY1_eBird']] == {{family_}}) |>
                                                                                         dplyr::group_by(.data[['model_param']]) |>
                                                                                         dplyr::summarize(
                                                                                           ST_and_LL_log = mean(.data[['ST_and_LL_log']])
                                                                                         ) |>
                                                                                         dplyr::arrange(-.data[['ST_and_LL_log']]) |>
                                                                                         dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_ST09_threshold_ST_and_energy_log_FAMILY <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                             dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                             dplyr::filter(.data[['FAMILY1_eBird']] == {{family_}}) |>
                                                                                             dplyr::group_by(.data[['model_param']]) |>
                                                                                             dplyr::summarize(
                                                                                               ST_and_energy_log = mean(.data[['ST_and_energy_log']])
                                                                                             ) |>
                                                                                             dplyr::arrange(-.data[['ST_and_energy_log']]) |>
                                                                                             dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_LL_FAMILY <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                            dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                              dplyr::filter(.data[['FAMILY1_eBird']] == {{family_}}) |>
                                                            dplyr::group_by(.data[['model_param']]) |>
                                                            dplyr::summarize(
                                                              weighted_mean_ll = mean(.data[['weighted_mean_ll']])
                                                            ) |>
                                                            dplyr::arrange(-.data[['weighted_mean_ll']]) |>
                                                            dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_distance_metric_FAMILY <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                         dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                           dplyr::filter(.data[['FAMILY1_eBird']] == {{family_}}) |>
                                                                         dplyr::group_by(.data[['model_param']]) |>
                                                                         dplyr::summarize(
                                                                           weighted_mean_win_distance = mean(.data[['weighted_mean_win_distance']])
                                                                         ) |>
                                                                         dplyr::arrange(-.data[['weighted_mean_win_distance']]) |>
                                                                         dplyr::ungroup()
      )$model_param[1], '.hdf5')

      best_param_by_LOO_energy_score_ORDER <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                             dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                             dplyr::filter(.data[['ORDER1_eBird']] == {{order_}}) |>
                                                                             dplyr::group_by(.data[['model_param']]) |>
                                                                             dplyr::summarize(
                                                                               weighted_energy_improvement = mean(.data[['weighted_energy_improvement']])
                                                                             ) |>
                                                                             dplyr::arrange(-.data[['weighted_energy_improvement']]) |>
                                                                             dplyr::ungroup()
      )$model_param[1], '.hdf5')
      
      best_param_by_LOO_ST09_threshold_LL_distance_score_ORDER <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                                 dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                                dplyr::filter(.data[['ORDER1_eBird']] == {{order_}}) |>
                                                                                                 dplyr::group_by(.data[['model_param']]) |>
                                                                                                 dplyr::summarize(
                                                                                                   ST09_threshold_LL_distance_score = mean(.data[['ST09_threshold_LL_distance_score']])
                                                                                                 ) |>
                                                                                                 dplyr::arrange(-.data[['ST09_threshold_LL_distance_score']]) |>
                                                                                                 dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_ST09_threshold_ST_and_LL_ORDER <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                         dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                        dplyr::filter(.data[['ORDER1_eBird']] == {{order_}}) |>
                                                                                         dplyr::group_by(.data[['model_param']]) |>
                                                                                         dplyr::summarize(
                                                                                           ST_and_LL = mean(.data[['ST_and_LL']])
                                                                                         ) |>
                                                                                         dplyr::arrange(-.data[['ST_and_LL']]) |>
                                                                                         dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_ST09_threshold_ST_and_energy_ORDER <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                             dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                            dplyr::filter(.data[['ORDER1_eBird']] == {{order_}}) |>
                                                                                             dplyr::group_by(.data[['model_param']]) |>
                                                                                             dplyr::summarize(
                                                                                               ST_and_energy = mean(.data[['ST_and_energy']])
                                                                                             ) |>
                                                                                             dplyr::arrange(-.data[['ST_and_energy']]) |>
                                                                                             dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_ST09_threshold_ST_and_LL_log_ORDER <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                        dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                        dplyr::filter(.data[['ORDER1_eBird']] == {{order_}}) |>
                                                                                        dplyr::group_by(.data[['model_param']]) |>
                                                                                        dplyr::summarize(
                                                                                          ST_and_LL_log = mean(.data[['ST_and_LL_log']])
                                                                                        ) |>
                                                                                        dplyr::arrange(-.data[['ST_and_LL_log']]) |>
                                                                                        dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_ST09_threshold_ST_and_energy_log_ORDER <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                            dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                                            dplyr::filter(.data[['ORDER1_eBird']] == {{order_}}) |>
                                                                                            dplyr::group_by(.data[['model_param']]) |>
                                                                                            dplyr::summarize(
                                                                                              ST_and_energy_log = mean(.data[['ST_and_energy_log']])
                                                                                            ) |>
                                                                                            dplyr::arrange(-.data[['ST_and_energy_log']]) |>
                                                                                            dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_LL_ORDER <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                   dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                   dplyr::filter(.data[['ORDER1_eBird']] == {{order_}}) |>
                                                                   dplyr::group_by(.data[['model_param']]) |>
                                                                   dplyr::summarize(
                                                                     weighted_mean_ll = mean(.data[['weighted_mean_ll']])
                                                                   ) |>
                                                                   dplyr::arrange(-.data[['weighted_mean_ll']]) |>
                                                                   dplyr::ungroup()
      )$model_param[1], '.hdf5')
      best_param_by_LOO_distance_metric_ORDER <- paste0(sp, '_2022_150km_', (raw_combined |> 
                                                                                dplyr::filter(.data[['sp']] != {{sp}}) |>
                                                                               dplyr::filter(.data[['ORDER1_eBird']] == {{order_}}) |>
                                                                                dplyr::group_by(.data[['model_param']]) |>
                                                                                dplyr::summarize(
                                                                                  weighted_mean_win_distance = mean(.data[['weighted_mean_win_distance']])
                                                                                ) |>
                                                                                dplyr::arrange(-.data[['weighted_mean_win_distance']]) |>
                                                                                dplyr::ungroup()
      )$model_param[1], '.hdf5')
    }
    
    if (include_taxomany_LOO){
      method_list <- list(
        'ST09_threshold_LL_distance_score'=best_model_by_ST09_threshold_LL_distance_score,
        'ST_and_LL'=best_model_by_ST_and_LL,
        'ST_and_energy'=best_model_by_ST_and_energy,
        'ST_and_LL_log'=best_model_by_ST_and_LL_log,
        'ST_and_energy_log'=best_model_by_ST_and_energy_log,
        'LL'=best_model_by_LL,
        'distance_metric'=best_model_by_distance_metric,
        'energy_score'=best_model_by_energy_score,
        'energy_score_days_integral'=best_model_by_energy_score_days_integral,
        'LOO_energy_score'=best_param_by_LOO_energy_score,
        'LOO_ST09_threshold_LL_distance_score'=best_param_by_LOO_ST09_threshold_LL_distance_score,
        'LOO_ST_and_LL'=best_param_by_LOO_ST_and_LL,
        'LOO_ST_and_energy'=best_param_by_LOO_ST_and_energy,
        'LOO_ST_and_LL_log'=best_param_by_LOO_ST_and_LL_log,
        'LOO_ST_and_energy_log'=best_param_by_LOO_ST_and_energy_log,
        'LOO_LL'=best_param_by_LOO_LL,
        'LOO_distance_metric'=best_param_by_LOO_distance_metric,
        'LOO_FAMILY_energy_score'=best_param_by_LOO_energy_score_FAMILY,
        'LOO_FAMILY_ST09_threshold_LL_distance_score'=best_param_by_LOO_ST09_threshold_LL_distance_score_FAMILY,
        'LOO_FAMILY_ST_and_LL'=best_param_by_LOO_ST_and_LL_FAMILY,
        'LOO_FAMILY_ST_and_energy'=best_param_by_LOO_ST_and_energy_FAMILY,
        'LOO_FAMILY_ST_and_LL_log'=best_param_by_LOO_ST_and_LL_log_FAMILY,
        'LOO_FAMILY_ST_and_energy_log'=best_param_by_LOO_ST_and_energy_log_FAMILY,
        'LOO_FAMILY_LL'=best_param_by_LOO_LL_FAMILY,
        'LOO_FAMILY_distance_metric'=best_param_by_LOO_distance_metric_FAMILY,
        'LOO_ORDER_energy_score'=best_param_by_LOO_energy_score_ORDER,
        'LOO_ORDER_ST09_threshold_LL_distance_score'=best_param_by_LOO_ST09_threshold_LL_distance_score_ORDER,
        'LOO_ORDER_ST_and_LL'=best_param_by_LOO_ST_and_LL_ORDER,
        'LOO_ORDER_ST_and_energy'=best_param_by_LOO_ST_and_energy_ORDER,
        'LOO_ORDER_ST_and_LL_log'=best_param_by_LOO_ST_and_LL_log_ORDER,
        'LOO_ORDER_ST_and_energy_log'=best_param_by_LOO_ST_and_energy_log_ORDER,
        'LOO_ORDER_LL'=best_param_by_LOO_LL_ORDER,
        'LOO_ORDER_distance_metric'=best_param_by_LOO_distance_metric_ORDER,
        'best_param_for_daves_multiobjective_5_sp'=best_param_by_daves_multiobjective
      )
    } else {
      method_list <- list(
        'ST09_threshold_LL_distance_score'=best_model_by_ST09_threshold_LL_distance_score,
        'ST_and_LL'=best_model_by_ST_and_LL,
        'ST_and_energy'=best_model_by_ST_and_energy,
        'ST_and_LL_log'=best_model_by_ST_and_LL_log,
        'ST_and_energy_log'=best_model_by_ST_and_energy_log,
        'LL'=best_model_by_LL,
        'distance_metric'=best_model_by_distance_metric,
        'energy_score'=best_model_by_energy_score,
        'energy_score_days_integral'=best_model_by_energy_score_days_integral,
        'LOO_energy_score'=best_param_by_LOO_energy_score,
        'LOO_ST09_threshold_LL_distance_score'=best_param_by_LOO_ST09_threshold_LL_distance_score,
        'LOO_ST_and_LL'=best_param_by_LOO_ST_and_LL,
        'LOO_ST_and_energy'=best_param_by_LOO_ST_and_energy,
        'LOO_ST_and_LL_log'=best_param_by_LOO_ST_and_LL_log,
        'LOO_ST_and_energy_log'=best_param_by_LOO_ST_and_energy_log,
        'LOO_LL'=best_param_by_LOO_LL,
        'LOO_distance_metric'=best_param_by_LOO_distance_metric,
        'best_param_for_daves_multiobjective_5_sp'=best_param_by_daves_multiobjective
      )
    }

    
    for (method in names(method_list)){
      values <- c(test_interval_based[test_interval_based$model==method_list[method],][,c('mean_ll_improvement','weighted_mean_ll_improvement',
                                                                                          'mean_win_distance_fraction', 'weighted_mean_win_distance',
                                                                                          'mean_energy_improvement', 'weighted_energy_improvement',
                                                                                          'pit_d','end_traverse_cor', 'end_traverse_cor_log',
                                                                                          "synth_routes_prebreeding_migration_straightness", "synth_routes_prebreeding_migration_n_stopovers", "synth_routes_prebreeding_migration_speed",
                                                                                          "synth_routes_breeding_straightness", "synth_routes_breeding_n_stopovers", "synth_routes_breeding_speed",
                                                                                          "synth_routes_postbreeding_migration_straightness", "synth_routes_postbreeding_migration_n_stopovers", "synth_routes_postbreeding_migration_speed",
                                                                                          "synth_routes_nonbreeding_straightness", "synth_routes_nonbreeding_n_stopovers", "synth_routes_nonbreeding_speed")])
      ## Add training sample size
      values$training_n_intervals <- train_interval_based$n_intervals[1]
      
      res <- list(
        sp=sp,
        method=method,
        model=method_list[[method]]
      )
      for (name in names(values)){
        res[name] <- values[name]
      }
      all_res[[length(all_res) + 1]] <- res
      
      if (all(c("straightness_diff_d", "n_stopovers_diff_d", "speed_diff_d", "straightness_diff", "n_stopovers_diff", "speed_diff") %in% names(train_interval_based))) {
        additoonal_biological_metrics <- train_interval_based[train_interval_based$model==method_list[method],][,c("straightness_diff_d", "n_stopovers_diff_d", "speed_diff_d", 
                                                                                                                   "straightness_diff", "n_stopovers_diff", "speed_diff",
                                                                                                                   "straightness", "n_stopovers", "speed")]
        for (name in names(additoonal_biological_metrics)){
          res[name] <- additoonal_biological_metrics[name]
        }
        all_res_with_tracking[[length(all_res_with_tracking) + 1]] <- res
      }
    }
  }
  
  all_res <- dplyr::bind_rows(all_res)
  all_res_with_tracking <- dplyr::bind_rows(all_res_with_tracking)
  print(paste0('Total species: ', length(all_res$sp |> unique())))
  print(paste0('Total species with tracking: ', all_res_with_tracking$sp |> unique() |> length()))
  
  return(list(all_res=all_res, all_res_with_tracking=all_res_with_tracking))
}

