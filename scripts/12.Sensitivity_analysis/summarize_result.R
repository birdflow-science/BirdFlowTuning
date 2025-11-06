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
all_res <- list()
for (sp in sp_list) {
  print(sp)
  for (trim_quantile in trim_quantile_list) {
    path_ <- paste0(glue::glue('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/150km_grid_search_trim{trim_quantile}'),'/',sp,'/',sp,'_150km_')
    train_df <- readRDS(paste0(path_,'/','eval_res_train.rds'))
    mean_weighted_mean_ll <- mean(train_df$weighted_mean_ll)
    all_res[[length(all_res)+1]] <- list(sp=sp, trim_quantile=trim_quantile, mean_weighted_mean_ll=mean_weighted_mean_ll)
  }
}

all_res <- do.call(rbind.data.frame, all_res)
all_res <- all_res|>
  dplyr::group_by(sp) |>
  dplyr::mutate(
    baseline = dplyr::first(mean_weighted_mean_ll[trim_quantile == 1]),
    relative_mean_weighted_mean_ll = mean_weighted_mean_ll - baseline
  ) |>
  dplyr::select(-baseline) |>
  dplyr::ungroup()

library(ggplot2)
p <- ggplot(data=all_res) + geom_line(aes(x=as.factor(trim_quantile), y=relative_mean_weighted_mean_ll, color=sp, group=sp)) +
  xlab('Abundance trim quantile') + ylab('Mean log-likelihood changes compared to no-trimming')
ggsave("../../data/10.Sensitivity_analysis/relative_ll_vs_trim.pdf", plot = p, width = 7, height = 5)

  