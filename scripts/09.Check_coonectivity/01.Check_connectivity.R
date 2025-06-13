library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/09.Check_coonectivity/')

# load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
# load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.

source('../02.Summarize_validation_preliminary/load_data_functions.R')

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
raw_combined <- raw_combined |>dplyr::filter(!is.na(.data[['abs_connectivity_diff']]))

write.csv(raw_combined, './raw_combined_MC.csv')


## LL
all_plots <- list()
for (sp in raw_combined$sp |> unique()) {
  tmp = raw_combined[raw_combined$sp==sp,]
  
  library(ggplot2)
  p <- ggplot(data=tmp, aes(x=mean_dist_cor, y=weighted_mean_ll_improvement, 
                        size=abs_connectivity_diff, color=abs_connectivity_diff)) + 
    geom_point() +
    scale_size_continuous(
      name  = "abs_connectivity_diff",
      range = c(6, 1)
    ) +
    scale_color_gradientn(
      name    = "abs_connectivity_diff",
      colours = rainbow(6)
    ) + labs(
      title    = sp,
      subtitle = "|Δconnectivity|",
      x        = "Cor with S&T",
      y        = "Weighted mean LL improvement",
    )
  print(p)
  all_plots[[length(all_plots)+1]] <- p
  Sys.sleep(1)
}

n_row <- ifelse(length(all_plots) %% 3>0, length(all_plots) %/% 3+1, length(all_plots) %/% 3)
pdf(glue::glue("./MC_LL.pdf"), width = 8*3, height = 4*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = all_plots, ncol = 3)  # Adjust 'ncol' to control the number of plots per row
dev.off()


#### Energy score

all_plots <- list()
for (sp in raw_combined$sp |> unique()) {
  tmp = raw_combined[raw_combined$sp==sp,]
  
  library(ggplot2)
  p <- ggplot(data=tmp, aes(x=mean_dist_cor, y=weighted_energy_improvement, 
                            size=abs_connectivity_diff, color=abs_connectivity_diff)) + 
    geom_point() +
    scale_size_continuous(
      name  = "abs_connectivity_diff",
      range = c(6, 1)
    ) +
    scale_color_gradientn(
      name    = "abs_connectivity_diff",
      colours = rainbow(6)
    ) + labs(
      title    = sp,
      subtitle = "|Δconnectivity|",
      x        = "Cor with S&T",
      y        = "Energy score improvement",
    )
  print(p)
  all_plots[[length(all_plots)+1]] <- p
  Sys.sleep(1)
}

n_row <- ifelse(length(all_plots) %% 3>0, length(all_plots) %/% 3+1, length(all_plots) %/% 3)
pdf(glue::glue("./MC_energy_score.pdf"), width = 8*3, height = 4*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = all_plots, ncol = 3)  # Adjust 'ncol' to control the number of plots per row
dev.off()


