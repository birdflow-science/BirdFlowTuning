library(ggplot2)
library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/04.Summarize_species_specific_tuning_bio_metrics/')
source('../plotting_params/plotting_params.R')
source('../02.Summarize_validation_preliminary/load_data_functions.R')

all_res_with_tracking <- read.csv('../../data/03.All_validation_summary/validation_compare_methods_tracking_sp_only_summary.csv')

## 01. Do conditional route stats
tmp <- all_res_with_tracking[all_res_with_tracking['method']=='ST098_and_LL',] ## ST098_and_LL as model selection method

all_conditional_rts_stats_res <- list()
MSE <- list()
for (line_count in 1:dim(tmp)[1]) {
  sp <- tmp[line_count, ]$sp
  print(sp)
  model_name <- tmp[line_count, ]$model
  model_path <- glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model_name}')
  bf <- import_birdflow(model_path)
  
  ##
  params <- list()
  params$species <- sp
  params$season <- 'prebreeding'
  real_track <- get_real_track(bf, params, filter=TRUE) #
  params$season <- 'postbreeding'
  real_track2 <- get_real_track(bf, params, filter=TRUE) #
  real_track$data <- rbind(real_track$data, real_track2$data)
  
  splitted_track <- split(real_track$data, real_track$data$route_id)
  all_route_stats <- list()
  for (i in seq_along(splitted_track)) {
    cat(sprintf("\rProcessing track %d/%d", i, length(splitted_track)))
    flush.console()
    track <- splitted_track[[i]]
    
    the_synth_track <- BirdFlowR::route(bf = bf, n = 10, 
                                        start = track$timestep[1], end=track$timestep[nrow(track)], 
                                        x_coord = track$x[1], y_coord = track$y[1], 
                                        from_marginals = TRUE)
    route_stats_synth <- rts_stats(the_synth_track)
    route_stats_synth <- as.data.frame(route_stats_synth)
    names(route_stats_synth) <- paste0(names(route_stats_synth), "_synth")
    
    route_stats_real <- rts_stats(list(data=track))
    route_stats_real <- as.data.frame(route_stats_real)
    names(route_stats_real) <- paste0(names(route_stats_real), "_real")
    all_route_stats[[length(all_route_stats) + 1]] <- cbind(route_stats_synth, route_stats_real)
  }
  
  conditional_rts_stats_res <- as.data.frame(do.call(rbind, all_route_stats))
  conditional_rts_stats_res$sp <- sp
  all_conditional_rts_stats_res[[length(all_conditional_rts_stats_res) + 1]] <- conditional_rts_stats_res
  

  mse_straightness <- mean(na.omit((conditional_rts_stats_res$straightness_real - conditional_rts_stats_res$straightness_synth))^2)
  mse_n_stopovers <- mean(na.omit((conditional_rts_stats_res$n_stopovers_real - conditional_rts_stats_res$n_stopovers_synth))^2)
  mse_speed <- mean(na.omit((conditional_rts_stats_res$speed_real - conditional_rts_stats_res$speed_synth))^2)
  
  MSE[[length(MSE) + 1]] <- list(sp=sp, mse_straightness=mse_straightness, mse_n_stopovers=mse_n_stopovers, mse_speed=mse_speed)
}

## Write files
trait_data <- read.csv('../../data/00.sp_info/All_combined_eco_function_traits.csv')
trait_data <- trait_data[,c('Common_Name1_eBird', 'ORDER1_eBird', 'FAMILY1_eBird', 'Species1_BirdLife', 'Species2_eBird', 'Species3_BirdTree')]

## Write MSE file
MSE_df <- do.call(rbind.data.frame, MSE) |> merge(
  ebirdst::ebirdst_runs[,c('species_code','common_name','nonbreeding_quality','prebreeding_migration_quality',
                           'breeding_quality','postbreeding_migration_quality')], 
  by.x = 'sp', by.y = 'species_code', all.x=T
) |> merge(
  trait_data, by.x = 'common_name', by.y = 'Common_Name1_eBird', all.x=T
)
write.csv(MSE_df, '../../data/05.Summarize_biological_metrics/route_stats_summary_MSE_df.csv')

## Write routes stats file
all_conditional_rts_stats_res <- do.call(rbind, all_conditional_rts_stats_res)
all_conditional_rts_stats_res <- all_conditional_rts_stats_res |> merge(
  ebirdst::ebirdst_runs[,c('species_code','common_name','nonbreeding_quality','prebreeding_migration_quality',
                           'breeding_quality','postbreeding_migration_quality')], 
  by.x = 'sp', by.y = 'species_code', all.x=T
) |> merge(
  trait_data, by.x = 'common_name', by.y = 'Common_Name1_eBird', all.x=T
)
write.csv(all_conditional_rts_stats_res, '../../data/05.Summarize_biological_metrics/route_stats_summary_bio_sanity_check.csv')


# birdflowroutes <- Routes(data, species=model$species, source='') |> as_BirdFlowRoutes(bf=model)
# plot(birdflowroutes)
# data <- readRDS('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/woothr/woothr_150km_interval_based_eval_using_migration_transitions/train_data_all_combined.rds')


### 02. Make plots
all_plots <- list()
for (sp in all_conditional_rts_stats_res$sp|>unique()) {
  conditional_rts_stats_res = all_conditional_rts_stats_res[all_conditional_rts_stats_res$sp==sp,]
  common_name <- conditional_rts_stats_res$common_name[1]
  p1 <- ggplot(data=conditional_rts_stats_res, aes(x=straightness_real, y=straightness_synth)) + 
    my_plotting_params[['scatter']] +
    geom_smooth(method='lm', color='steelblue') +
    geom_abline(intercept = 0,
                slope = 1,
                color = "red2",
                linetype = "dashed") +
    ggtitle(glue::glue("{common_name}: Straightness")) +
    labs(x = 'Observed straightness', y = 'Modeled straightness') +
    my_plotting_params[['theme']] +
    my_plotting_params[['formater']]
  
  all_plots[[length(all_plots) + 1]] <- p1
  
  p2 <- ggplot(data=conditional_rts_stats_res, aes(x=n_stopovers_real, y=n_stopovers_synth)) + 
    my_plotting_params[['scatter_with_xjitter']] + 
    geom_smooth(method='lm', color='steelblue') +
    geom_abline(intercept = 0,
                slope = 1,
                color = "red2",
                linetype = "dashed") +
    ggtitle(glue::glue("{common_name}: Number of stopovers")) +
    labs(x = 'Observed number of stopovers', y = 'Modeled number of stopovers') +
    my_plotting_params[['theme']] +
    my_plotting_params[['formater']]
  
  all_plots[[length(all_plots) + 1]] <- p2
  
  p3 <- ggplot(data=conditional_rts_stats_res, aes(x=speed_real/1000, y=speed_synth/1000)) + 
    my_plotting_params[['scatter']] +
    geom_smooth(method='lm', color='steelblue') +
    geom_abline(intercept = 0,
                slope = 1,
                color = "red2",
                linetype = "dashed") +
    ggtitle(glue::glue("{common_name}: Migration speed")) +
    labs(x = 'Observed migration speed (km/day)', y = 'Modeled migration speed (km/day)') +
    my_plotting_params[['theme']] +
    my_plotting_params[['formater']]
  
  all_plots[[length(all_plots) + 1]] <- p3

}

library(gridExtra)
n_row <- 6
n_col <- 3
pdf(glue::glue("../../data/05.Summarize_biological_metrics/bio_sanity_check.pdf"), width = my_plotting_params[['single_plot_width']]*1*n_col, height = my_plotting_params[['single_plot_height']]*1.2*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = all_plots, ncol = n_col)  # Adjust 'ncol' to control the number of plots per row
dev.off()

