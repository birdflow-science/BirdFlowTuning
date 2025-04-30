library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/08.Biological_fact_check/')

devtools::install_local("/home/yc85_illinois_edu/BirdFlowR", force = T, dependencies = FALSE) # if the BirdFlowR is updated, we need to reinstall it, so that i can be used in BirdFlowPipeline!
devtools::install_local("/home/yc85_illinois_edu/BirdFlowPipeline", force = T, dependencies = FALSE)

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
# tmp <- all_res_with_tracking[all_res_with_tracking['method']=='LL',] ## LL as model selection method
tmp <- all_res_with_tracking[all_res_with_tracking['method']=='ST_and_LL_log',] ## LL as model selection method

all_conditional_rts_stats_res <- list()
all_plots <- list()
MSE <- list()
for (line_count in 1:dim(tmp)[1]) {
  sp <- tmp[line_count, ]$sp
  print(sp)
  model_name <- tmp[line_count, ]$model
  model_path <- glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model_name}')
  bf <- import_birdflow(model_path)
  # my_routes <- route(bf, n=100)
  # p <- plot(my_routes)
  # print(p)
  
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
  all_conditional_rts_stats_res[[length(all_conditional_rts_stats_res) + 1]] <- conditional_rts_stats_res
  
  p1 <- ggplot(data=conditional_rts_stats_res, aes(x=straightness_real, y=straightness_synth)) + 
    geom_point() +
    geom_smooth(method='lm') +
    geom_abline(intercept = 0,
                slope = 1,
                color = "red",
                linetype = "dashed") +
    ggtitle(glue::glue("{sp}: Straightness"))
  all_plots[[length(all_plots) + 1]] <- p1
  mse_straightness <- mean(na.omit((conditional_rts_stats_res$straightness_real - conditional_rts_stats_res$straightness_synth))^2)
  
  p2 <- ggplot(data=conditional_rts_stats_res, aes(x=n_stopovers_real, y=n_stopovers_synth)) + 
    geom_point() +
    geom_jitter(width = 0.1, height = 0, alpha = 0.6) + 
    geom_smooth(method='lm') +
    geom_abline(intercept = 0,
                slope = 1,
                color = "red",
                linetype = "dashed") +
    ggtitle(glue::glue("{sp}: Number of stopovers"))
  all_plots[[length(all_plots) + 1]] <- p2
  mse_n_stopovers <- mean(na.omit((conditional_rts_stats_res$n_stopovers_real - conditional_rts_stats_res$n_stopovers_synth))^2)
  
  p3 <- ggplot(data=conditional_rts_stats_res, aes(x=speed_real, y=speed_synth)) + 
    geom_point() +
    geom_smooth(method='lm') +
    geom_abline(intercept = 0,
                slope = 1,
                color = "red",
                linetype = "dashed") +
    ggtitle(glue::glue("{sp}: Migration speed"))
  all_plots[[length(all_plots) + 1]] <- p3
  mse_speed <- mean(na.omit((conditional_rts_stats_res$speed_real - conditional_rts_stats_res$speed_synth))^2)
  
  MSE[[length(MSE) + 1]] <- list(sp=sp, mse_straightness=mse_straightness, mse_n_stopovers=mse_n_stopovers, mse_speed=mse_speed)
}

a <- do.call(rbind.data.frame, MSE)
b <- do.call(rbind.data.frame, MSE)

a
b

all_conditional_rts_stats_res <- do.call(rbind, all_conditional_rts_stats_res)
write.csv(all_conditional_rts_stats_res, '../../data/03.All_validation_summary/sanity_check.csv')

library(gridExtra)
n_row <- 6
n_col <- 3
pdf(glue::glue("../../data/plot/bio_sanity_check.pdf"), width = 4*n_col, height = 4*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = all_plots, ncol = n_col)  # Adjust 'ncol' to control the number of plots per row
dev.off()

# conditional_rts_stats_res <- conditional_rts_stats_res[!is.na(conditional_rts_stats_res$straightness),]
# route_stats <- colMeans(conditional_rts_stats_res)
# route_stats_list <- list()
# for (name in names(route_stats)){
#   route_stats_list[[name]] <- route_stats[[name]]
# }
# route_stats <- route_stats_list


