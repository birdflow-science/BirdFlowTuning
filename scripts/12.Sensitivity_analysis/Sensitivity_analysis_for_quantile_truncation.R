library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)
setwd('~/BirdFlow_Validation_Project/scripts/12.Sensitivity_analysis')
load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.
load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.


## 02. Define species
sp_list <- c('amewoo', 'brwhaw', 'buwtea', 'lobcur', 'woothr')
#
trim_quantile_list <-  c(1, 0.99, 0.98, 0.95)

for (sp in sp_list) {
  print(glue::glue('====================== Looking at {sp} ... ======================'))
  # if (sp=='amekes') {
  #   next
  # }
  
  if (!sp %in% ebirdst::ebirdst_runs$species_code) {
    print(glue::glue('{sp} not in ebirdst product'))
    next
  }
  
  if (ebirdst::ebirdst_runs[ebirdst::ebirdst_runs$species_code==sp,]$is_resident) {
    print(glue::glue('{sp} is resident.. skipping'))
    next
  }
  
  ebird_info <- ebirdst::ebirdst_runs[ebirdst::ebirdst_runs$species_code==sp,]
  if (!all(!is.na(c(ebird_info$postbreeding_migration_start, ebird_info$postbreeding_migration_end, ebird_info$prebreeding_migration_start, ebird_info$prebreeding_migration_end, ebird_info$breeding_start, ebird_info$breeding_end, ebird_info$nonbreeding_start, ebird_info$nonbreeding_end)))) {
    print('Some seasons starts and ends are not clear based on eBird, so skipping...')
    next
  }
  

  for (trim_quantile in trim_quantile_list) {
  
    hdf_output_path <- paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/150km_grid_search_trim{trim_quantile}'),'/',sp)
    batchtools_output_path <- paste0(glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/150km_grid_search_trim{trim_quantile}'),'/',sp)
    
    if (file.exists(glue::glue('{hdf_output_path}/{sp}_150km_/eval_res_test.rds'))) {
      if (length(list.files(glue::glue('{hdf_output_path}/{sp}_150km'))) > 220) {
        print(glue::glue('{sp} already finished!'))
        next
      } else {
        print(glue::glue('{sp} seems finished but too many models failed training so rerun it!'))
      }
    }
    
    if (!dir.exists(hdf_output_path)){dir.create(hdf_output_path, recursive = TRUE)}
    if (!dir.exists(batchtools_output_path)){dir.create(batchtools_output_path, recursive = TRUE)}
    
    tryCatch({
      ## 02. Fit batch models
      my_batch_trainer <- BatchBirdFlowTrainer(sp,
                                               skip_quality_checks = TRUE,
                                               res = 150,
                                               gpu_ram = 30, hdf_path = hdf_output_path, base_output_path = batchtools_output_path,
                                               suffix='',
                                               trim_quantile=trim_quantile)
      
      saveRDS(my_batch_trainer$params, file.path(my_batch_trainer$params$output_path, 'params.rds'))
      # write.csv(as.data.frame(batchtools::getJobStatus()), paste0(sp_output_path, 'training_JobStatus.csv'))
      
      ## 03. Import transition data
      ### Transition loading function
      get_transitions_prepared <- function(loader) {
        BirdFlowPipeline::validate_TransitionsLoader(loader)
        
        ## Read the combined data
        combined_interval_path <- file.path('/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/combined_data/BirdFlowIntervals/150km', paste0(loader$batch_trainer$params$species, '.hdf5'))
        interval_obj <- BirdFlowR::read_intervals(combined_interval_path)
        # Filter intervals to ask at least one leg in the migration season
        target_timesteps <- c(BirdFlowR::lookup_season_timesteps(loader$batch_trainer$bf, season='prebreeding'), 
                              BirdFlowR::lookup_season_timesteps(loader$batch_trainer$bf, season='postbreeding'))
        interval_obj$data <- interval_obj$data[(interval_obj$data$timestep1 %in% target_timesteps) | (interval_obj$data$timestep2 %in% target_timesteps),]
        interval_obj$data <- interval_obj$data[(interval_obj$data$lon1>-180) & (interval_obj$data$lon1<180) & (interval_obj$data$lon2>-180) & (interval_obj$data$lon2<180),]
        interval_obj$data <- interval_obj$data[(interval_obj$data$lat1>-90) & (interval_obj$data$lat1<90) & (interval_obj$data$lat2>-90) & (interval_obj$data$lat2<90),]
        
        ## Get the one-week transition
        interval_one_week_obj <- interval_obj
        interval_one_week_obj$data <-interval_one_week_obj$data[interval_one_week_obj$data$timestep2 - interval_one_week_obj$data$timestep1 == 1,]
        interval_one_week_obj$data <- interval_one_week_obj$data[(interval_one_week_obj$data$timestep1 %in% target_timesteps) | (interval_one_week_obj$data$timestep2 %in% target_timesteps),]
        
        if ( nrow(interval_obj$data)>1000) {
          interval_obj$data <- interval_obj$data |> dplyr::sample_n(1000, replace=FALSE)
        }
        if ( nrow(interval_one_week_obj$data)>300) {
          interval_one_week_obj$data <- interval_one_week_obj$data |> dplyr::sample_n(300, replace=FALSE)
        }
        
        return(list(interval_obj=interval_obj,
                    interval_one_week_obj=interval_one_week_obj))
      }
    
      data_loader <- TransitionsLoader(my_batch_trainer)
      data_loader <- data_loader |> load_data(loading_function=get_transitions_prepared) # Here you can customize the loading_function for the transition data
      
      if ((nrow(data_loader$transitions$interval_obj$data) < 28) | (nrow(data_loader$transitions$interval_one_week_obj$data) < 10)) {
        print(glue::glue('{sp} does not have enough data for training!'))
        if (dir.exists(hdf_output_path)){unlink(hdf_output_path, recursive = TRUE)}
        if (dir.exists(batchtools_output_path)){unlink(batchtools_output_path, recursive = TRUE)}
        next
      }
      
      tryCatch({
        # Fit
        my_batch_trainer <- fit(my_batch_trainer) #, force_refit=TRUE) # Fit the model with parameter grid
      }, error = function (e) {
        message("Training contains error for ", sp, ": ", e$message)
      })
      
      files <- list.files(path = my_batch_trainer$params$hdf_dir,
                          pattern = paste0('^', my_batch_trainer$params$species, '.*',my_batch_trainer$params$res, 'km_.*\\.hdf5$'),
                          full.names = TRUE)
      for (file in files) {
        loading_res <- BirdFlowPipeline::identify_hdf5_model(file)
      }
      
      if (length(list.files(glue::glue('{hdf_output_path}/{sp}_150km'))) < 220) {
        print(glue::glue('{sp} seems finished but too many models failed! Next one...'))
        next
      }
      
      # Save the transition data
      saveRDS(data_loader$transitions, file.path(data_loader$batch_trainer$params$output_path, 'transitions.rds'))
      
      ## 04. Train test split
      split_data <- data_loader |> 
        split_data(splitting_function=train_test_split, seed=42) # Here you can customize the train_test_split function
      
      ## 05. Evaluate training set
      my_evaluator <- BatchBirdFlowEvaluator(my_batch_trainer)
      tryCatch({
        eval_res_train <- evaluate(my_evaluator, data=split_data$training_data, evaluation_function=evaluate_model)
      }, error = function (e) {
        message("Validation (step 1) error for ", sp, ": ", e$message)
      })
      
      
      # Save metrics for each transition each model
      all_transitions <- list()
      for (this_model in eval_res_train) {
        this_transition_df <- this_model$metric_for_each_transition
        this_transition_df$model <- this_model$df$model
        all_transitions[[length(all_transitions) + 1]] <- this_transition_df
      }
      
      all_transitions <- do.call(rbind, all_transitions)
      saveRDS(all_transitions, file.path(my_evaluator$batch_trainer$params$output_path, glue::glue('Training_each_transition_evaluation.rds')))
      
      # Get one score summary for each model
      eval_res_train <- eval_res_train |> lapply(function(i){i$df}) |>
        data.table::rbindlist(fill = TRUE) |>
        tibble::as_tibble() |>
        dplyr::arrange(dplyr::desc(.data$mean_ll))
      saveRDS(eval_res_train, file.path(my_evaluator$batch_trainer$params$output_path, glue::glue('eval_res_train.rds')))
      
      ## 06. Evaluate test set
      tryCatch({
        eval_res_test <- evaluate(my_evaluator, data=split_data$test_data, evaluation_function=evaluate_model)
      }, error = function (e) {
        message("Validation (step 2) error for ", sp, ": ", e$message)
      })
    
      # Save metrics for each transition each model
      all_transitions <- list()
      for (this_model in eval_res_test) {
        this_transition_df <- this_model$metric_for_each_transition
        this_transition_df$model <- this_model$df$model
        all_transitions[[length(all_transitions) + 1]] <- this_transition_df
      }
        
      all_transitions <- do.call(rbind, all_transitions)
      saveRDS(all_transitions, file.path(my_evaluator$batch_trainer$params$output_path, glue::glue('Test_each_transition_evaluation.rds')))
      
      eval_res_test <- eval_res_test |> lapply(function(i){i$df}) |>
        data.table::rbindlist(fill = TRUE) |>
        tibble::as_tibble() |>
        dplyr::arrange(dplyr::desc(.data$mean_ll))
      saveRDS(eval_res_test, file.path(my_evaluator$batch_trainer$params$output_path, glue::glue('eval_res_test.rds')))
      
      ## Cp the batchtool intermediate files to /project
      # dir.create(glue::glue('{hdf_output_path}/{sp}_150km_'), recursive = TRUE, showWarnings = T)
      R.utils::copyDirectory(glue::glue('{batchtools_output_path}/{sp}_150km_'), glue::glue('{hdf_output_path}/{sp}_150km_'))
      
      message('This is the end for ', sp, '! Great!')
      
    }, error = function(e) {
      message("An error occurred: ", conditionMessage(e))
    })
  }
}


