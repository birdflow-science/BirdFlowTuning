library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/02.Summarize_validation_preliminary/')
source('load_data_functions.R')
# source('my_distribution_performance.R')
# devtools::install_local("/home/yc85_illinois_edu/BirdFlowR", force = T, dependencies = FALSE) # if the BirdFlowR is updated, we need to reinstall it, so that i can be used in BirdFlowPipeline!
# devtools::install_local("/home/yc85_illinois_edu/BirdFlowPipeline", force = T, dependencies = FALSE)
load_all("/home/yc85_illinois_edu/BirdFlowPipeline")

## load data
# res <- load_raw_validation_all_sp()
# raw_combined <- res[['raw_combined']]
# raw_combined_with_tracking <-  res[['raw_combined_with_tracking']]
# 
# res <- load_best_models_validation_all_sp(raw_combined, raw_combined_with_tracking)
# all_res <- res[['all_res']]
# all_res_with_tracking <- res[['all_res_with_tracking']]

all_res <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
raw_combined <- read.csv('../../data/03.All_validation_summary/validation_all_models_gridsearch_summary.csv')

res <- raw_combined |> dplyr::group_by(sp) |> 
  dplyr::arrange(-.data[['weighted_mean_ll_improvement']]) |> 
  dplyr::slice(1) |>
  dplyr::ungroup()

hist(res$mean_dist_cor, breaks=20)
hist(res$traverse_cor, breaks=20)

##
sp <- 'cangoo'
tmp <- raw_combined[raw_combined$sp==sp,]

##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'),
         (tmp[(tmp['mean_dist_cor']>0.8) & (tmp['mean_dist_cor']<0.9),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))

##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp[(tmp['mean_dist_cor']>0.9) & (tmp['mean_dist_cor']<0.95),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))

##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp[(tmp['mean_dist_cor']>0.95) & (tmp['mean_dist_cor']<0.97),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))

##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp[(tmp['mean_dist_cor']>0.97) & (tmp['mean_dist_cor']<0.99),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))

##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp[(tmp['mean_dist_cor']>0.98) & (tmp['mean_dist_cor']<0.99),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))

##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'),
         (tmp[(tmp['mean_dist_cor']>0.99) & (tmp['mean_dist_cor']<0.995),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))

##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'),
         (tmp[(tmp['mean_dist_cor']>0.995) & (tmp['mean_dist_cor']<1),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))


##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp[(tmp['mean_dist_cor']>0.8),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(BirdFlowR::route(
  model,
  n=500))

##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp[(tmp['mean_dist_cor']>0.9),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(BirdFlowR::route(
  model,
  n=500))


##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp[(tmp['mean_dist_cor']>0.98),] |> dplyr::arrange(-.data[['weighted_energy_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))

##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp[(tmp['mean_dist_cor']>0.93),] |> dplyr::arrange(-.data[['weighted_energy_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))


##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp[(tmp['mean_dist_cor']>0.9),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=20)))

xy = BirdFlowR::latlon_to_xy(lat=19, lon=-101, bf=model)
plot(BirdFlowR::route(
  model,
  n=20,
  x_coord=xy[['x']],
  y_coord=xy[['y']]))

xy = BirdFlowR::latlon_to_xy(lat=21, lon=-80, bf=model)
plot(BirdFlowR::route(
  model,
  n=20,
  x_coord=xy[['x']],
  y_coord=xy[['y']],
  direction='backward'))

xy = BirdFlowR::latlon_to_xy(lat=52, lon=-101, bf=model)
plot(BirdFlowR::route(
  model,
  n=20,
  x_coord=xy[['x']],
  y_coord=xy[['y']],
  start=30,
  end=1,
  direction='backward'))

xy = BirdFlowR::latlon_to_xy(lat=45, lon=-75, bf=model)
plot(BirdFlowR::route(
  model,
  n=20,
  x_coord=xy[['x']],
  y_coord=xy[['y']],
  start=30,
  end=1,
  direction='backward'))

##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp[(tmp['mean_dist_cor']>0.98),] |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))

xy = BirdFlowR::latlon_to_xy(lat=19, lon=-101, bf=model)
plot(BirdFlowR::route(
  model,
  n=20,
  x_coord=xy[['x']],
  y_coord=xy[['y']]))

xy = BirdFlowR::latlon_to_xy(lat=52, lon=-101, bf=model)
plot(BirdFlowR::route(
  model,
  n=20,
  x_coord=xy[['x']],
  y_coord=xy[['y']],
  start=30,
  end=1,
  direction='backward'))


##
model <- BirdFlowR::import_birdflow(
  paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), 
         (tmp |> dplyr::arrange(-.data[['weighted_mean_ll_improvement']]))$model[1])
)
plot(plot(BirdFlowR::route(
  model,
  n=500)))


########## Plotting
######## Part 0: end_traverse_cor vs. LL improvement
# plot(raw_combined[raw_combined$sp=='treswa',]$end_traverse_cor_log, raw_combined[raw_combined$sp=='treswa',]$weighted_mean_ll_improvement)
library(ggplot2)
# build the plot with one small‐multiple per species
p <- ggplot(raw_combined, 
            aes(x = traverse_cor, 
                y = weighted_mean_ll_improvement)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ sp, scales = "free") +
  labs(
    x = "traverse_cor",
    y = "Weighted mean LL improvement"
  ) +
  theme_minimal()

# save to PDF (all facets on one page)
ggsave("species_plots_traverse_cor.pdf", p, 
       device = "pdf", 
       width  = 50, 
       height = 50, limitsize = FALSE)


p <- ggplot(raw_combined, 
            aes(x = mean_dist_cor, 
                y = weighted_mean_ll_improvement)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ sp, scales = "free") +
  labs(
    x = "mean_dist_cor",
    y = "Weighted mean LL improvement"
  ) +
  theme_minimal()

# save to PDF (all facets on one page)
ggsave("species_plots_mean_cor.pdf", p, 
       device = "pdf", 
       width  = 50, 
       height = 50, limitsize = FALSE)


p <- ggplot(raw_combined, 
            aes(x = weighted_mean_ll_improvement, 
                y = pit_d)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ sp, scales = "free") +
  labs(
    x = "weighted_mean_ll_improvement",
    y = "pit_d"
  ) +
  theme_minimal()

# save to PDF (all facets on one page)
ggsave("species_plots_LL_and_pit_d.pdf", p, 
       device = "pdf", 
       width  = 50, 
       height = 50, limitsize = FALSE)


# ######## part 1: prediction metrics
# ## plot 1
# 
# for (metric in names(all_res)[4:length(names(all_res))]){
#   # Boxplot for ll_improvement by method
#   p1 <- ggplot(all_res, aes(x = method, y = .data[[metric]])) +
#     geom_boxplot() +
#     labs(title = metric, x = "Method", y = metric) +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1),
#           plot.margin = margin(b = 20, unit = "pt"))
#   print(p1)
# }

# ## with cor-S&T > 0.9 and. < 0.9, will the model perform differently?
# library(ggpubr)
# tmp <- all_res[all_res['method']=='LL',]
# tmp$end_traverse_cor_over_09 <- tmp$end_traverse_cor>0.9
# ggplot(data=tmp, aes(x=end_traverse_cor_over_09, y=weighted_mean_ll_improvement)) +
#   geom_boxplot() +
#   stat_compare_means(method = "t.test", label = "p.format")
# 
# ggplot(data=all_res, aes(x=method, y=end_traverse_cor)) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1),
#         plot.margin = margin(b = 20, unit = "pt"))
# 

library(ggplot2)
### plot 2: mean_ll_improvement & mean_ll_improvement_quantile
ggplot(data=all_res, aes(x=.data[['mean_ll_improvement']], y=.data[['mean_win_distance_fraction']], color=.data[['method']])) + 
  geom_point()

### plot 3
assign_color_category <- function(method) {
  if (grepl("ST09_threshold_LL_distance", method)) {
    "ST09_threshold_LL_distance"
  } else if (grepl("ST_and_LL_log", method)) {
    "ST_and_LL_log"
  } else if (grepl("ST_and_energy_log", method)) {
    "ST_and_energy_log"
  } else if (grepl("ST085_and_LL", method)) {
    "ST085_and_LL"
  } else if (grepl("ST085_and_energy_score", method)) {
    "ST085_and_energy_score"
  } else if (grepl("ST090_and_LL", method)) {
    "ST090_and_LL"
  } else if (grepl("ST090_and_energy_score", method)) {
    "ST090_and_energy_score"
  } else if (grepl("ST092_and_LL", method)) {
    "ST092_and_LL"
  } else if (grepl("ST092_and_energy_score", method)) {
    "ST092_and_energy_score"
  } else if (grepl("ST095_and_LL", method)) {
    "ST095_and_LL"
  } else if (grepl("ST095_and_energy_score", method)) {
    "ST095_and_energy_score"
  } else if (grepl("ST097_and_LL", method)) {
    "ST097_and_LL"
  } else if (grepl("ST097_and_energy_score", method)) {
    "ST097_and_energy_score"
  } else if (grepl("ST098_and_LL", method)) {
    "ST098_and_LL"
  } else if (grepl("ST098_and_energy_score", method)) {
    "ST098_and_energy_score"
  } else if (grepl("ST099_and_LL", method)) {
    "ST099_and_LL"
  } else if (grepl("ST099_and_energy_score", method)) {
    "ST099_and_energy_score"
  } else if (grepl("ST_and_LL", method)) {
    "ST_and_LL"
  } else if (grepl("ST_and_energy", method)) {
    "ST_and_energy"
  } else if (grepl("energy_score_days_integral", method)) {
    "energy_score_days_integral"
  } else if (grepl("energy_score", method)) {
    "energy_score"
  } else if (grepl("distance_metric", method)) {
    "distance_metric"
  } else if (grepl("LL", method)) {
    "LL"
  } else if (grepl("multiobjective", method)) {
    "best_param_for_daves_multiobjective_7_sp"
  } else {
    "Other"
  }
}

a <- all_res |>
  dplyr::group_by(.data[['method']]) |>
  dplyr::select(c('weighted_mean_ll_improvement','weighted_mean_win_distance', 'pit_d', 'mean_dist_cor')) |>
  dplyr::summarise(
    mean_weighted_mean_ll_improvement=mean(.data[['weighted_mean_ll_improvement']]),
    mean_weighted_mean_win_distance=mean(.data[['weighted_mean_win_distance']]),
    mean_pit_d=mean(.data[['pit_d']]),
    mean_mean_dist_cor=mean(.data[['mean_dist_cor']])
  ) |>
  dplyr::mutate(color_category = sapply(method, assign_color_category))


ggplot(data = a, 
       aes(x = mean_weighted_mean_ll_improvement, 
           y = mean_weighted_mean_win_distance, 
           color = color_category,
           shape = factor(
             ifelse(grepl("daves_multiobjective", method),
                    "Others",
               ifelse(grepl("^LOO", method), "LOO", "Species-specific")
             )
           ))) + 
  geom_point(size = 10, alpha=0.8) +
  scale_shape_manual(name = "Method type", 
                     values = c("LOO" = 17, "Species-specific" = 16, "Others"=15))

ggplot(data = a[a$method %in% c('ST_and_LL',
                                'ST085_and_LL',
                                'ST090_and_LL',
                                'ST092_and_LL',
                                'ST095_and_LL',
                                'ST097_and_LL',
                                'ST098_and_LL',
                                'ST099_and_LL',
                                'LOO_ST_and_LL',
                                'LOO_ST085_and_LL',
                                'LOO_ST090_and_LL',
                                'LOO_ST092_and_LL',
                                'LOO_ST095_and_LL',
                                'LOO_ST097_and_LL',
                                'LOO_ST098_and_LL',
                                'LOO_ST099_and_LL'
                                ),], 
       aes(x = mean_weighted_mean_ll_improvement, 
           y = mean_weighted_mean_win_distance, 
           color = color_category,
           shape = factor(
             ifelse(grepl("daves_multiobjective", method),
                    "Others",
                    ifelse(grepl("^LOO", method), "LOO", "Species-specific")
             )
           ))) + 
  geom_point(size = 10, alpha=0.8) +
  scale_shape_manual(name = "Method type", 
                     values = c("LOO" = 17, "Species-specific" = 16, "Others"=15))

tmp = a[a$method %in% c('ST085_and_LL',
                        'ST090_and_LL',
                        'ST092_and_LL',
                        'ST095_and_LL',
                        'ST097_and_LL',
                        'ST098_and_LL',
                        'ST099_and_LL'),]
ggplot(data=tmp, 
       aes(x=as.factor(method), 
           y=mean_weighted_mean_ll_improvement)
       ) + geom_bar()

# sp <- 'amered'
# bf <- BirdFlowR::import_birdflow(paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), all_res[(all_res$method=='LL') & (all_res$sp==sp),]$model))
# BirdFlowR::distribution_performance(bf, metrics = 'mean_distr_cor', season = 'prebreeding', log=T)$mean_distr_cor
# plot(BirdFlowR::route(bf, n=100))
# bf <- BirdFlowR::import_birdflow(paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/'), all_res[(all_res$method=='ST_and_LL') & (all_res$sp==sp),]$model))
# BirdFlowR::distribution_performance(bf, metrics = 'mean_distr_cor', season = 'prebreeding', log=T)$mean_distr_cor
# plot(BirdFlowR::route(bf, n=100))


### plot 3
ggplot(data = a, 
       aes(x = color_category, 
           y = mean_pit_d, 
           fill = factor(
             ifelse(grepl("daves_multiobjective", method),
                    "Others",
                    ifelse(grepl("^LOO", method), "LOO", "Species-specific")
             )
           ))) +
  geom_bar(stat = "identity", 
           position = position_dodge2(width = 0.9, preserve = "single"), 
           alpha = 0.8) +
  scale_fill_manual(name = "Method type", 
                    values = c("LOO" = "dodgerblue", "Species-specific" = "orange", "Others" = "grey")) +
  labs(x = "Model selection method", y = "Mean Pit D") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(min(a[['mean_pit_d']]) - 0.5*sd(a[['mean_pit_d']]), max(a[['mean_pit_d']]) + 0.5*sd(a[['mean_pit_d']])))

### plot 4
ggplot(data = a, 
       aes(x = color_category, 
           y = mean_end_traverse_cor, 
           fill = factor(
             ifelse(grepl("daves_multiobjective", method),
                    "Others",
                    ifelse(grepl("^LOO", method), "LOO", "Species-specific")
             )
           ))) +
  geom_bar(stat = "identity", 
           position = position_dodge2(width = 0.9, preserve = "single"), 
           alpha = 0.8) +
  scale_fill_manual(name = "Method type", 
                    values = c("LOO" = "dodgerblue", "Species-specific" = "orange", "Others" = "grey")) +
  labs(x = "Model selection method", y = "mean_end_traverse_cor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(min(a[['mean_end_traverse_cor']]) - 0.5*sd(a[['mean_end_traverse_cor']]), max(a[['mean_end_traverse_cor']]) + 0.5*sd(a[['mean_end_traverse_cor']])))


########## Plotting, part 2: biological metrics
# Plot 1
a <- all_res_with_tracking |>
  dplyr::group_by(.data[['method']]) |>
  dplyr::select(c('straightness_diff_d','n_stopovers_diff_d', 'speed_diff_d', 
                  'straightness_diff','n_stopovers_diff', 'speed_diff')) |>
  dplyr::summarise(
    straightness_diff_d=mean(.data[['straightness_diff_d']]),
    n_stopovers_diff_d=mean(.data[['n_stopovers_diff_d']]),
    speed_diff_d=mean(.data[['speed_diff_d']]),
    straightness_diff=mean(.data[['straightness_diff']]),
    n_stopovers_diff=mean(.data[['n_stopovers_diff']]),
    speed_diff=mean(.data[['speed_diff']]),
  ) |>
  dplyr::mutate(color_category = sapply(method, assign_color_category))

for (metric in c('straightness_diff_d','n_stopovers_diff_d', 'speed_diff_d', 
                 'straightness_diff','n_stopovers_diff', 'speed_diff')){
  p <- ggplot(data = a, 
         aes(x = color_category, 
             y = .data[[metric]], 
             fill = factor(
               ifelse(grepl("daves_multiobjective", method),
                      "Others",
                      ifelse(grepl("^LOO", method), "LOO", "Species-specific")
               )
             ))) +
    geom_bar(stat = "identity", 
             position = position_dodge2(width = 0.9, preserve = "single"), 
             alpha = 0.8) +
    scale_fill_manual(name = "Method type", 
                      values = c("LOO" = "dodgerblue", "Species-specific" = "orange", "Others" = "grey")) +
    labs(x = "Model selection method", y = metric, title = metric) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    coord_cartesian(ylim = c(min(a[[metric]]) - 0.5*sd(a[[metric]]), max(a[[metric]]) + 0.5*sd(a[[metric]])))
  
  print(p)
}

# Plot 2
a <- all_res |>
  dplyr::group_by(.data[['method']]) |>
  dplyr::select(c('synth_routes_nonbreeding_speed', 'synth_routes_breeding_speed')) |>
  dplyr::summarise(
    synth_routes_nonbreeding_speed=mean(.data[['synth_routes_nonbreeding_speed']]),
    synth_routes_breeding_speed=mean(.data[['synth_routes_breeding_speed']])
  ) |>
  dplyr::mutate(color_category = sapply(method, assign_color_category))

for (metric in c('synth_routes_nonbreeding_speed', 'synth_routes_breeding_speed')){
  p <- ggplot(data = a, 
              aes(x = color_category, 
                  y = .data[[metric]], 
                  fill = factor(
                    ifelse(grepl("daves_multiobjective", method),
                           "Others",
                           ifelse(grepl("^LOO", method), "LOO", "Species-specific")
                    )
                  ))) +
    geom_bar(stat = "identity", 
             position = position_dodge2(width = 0.9, preserve = "single"), 
             alpha = 0.8) +
    scale_fill_manual(name = "Method type", 
                      values = c("LOO" = "dodgerblue", "Species-specific" = "orange", "Others" = "grey")) +
    labs(x = "Model selection method", y = metric, title = metric) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    coord_cartesian(ylim = c(min(a[[metric]]) - 0.5*sd(a[[metric]]), max(a[[metric]]) + 0.5*sd(a[[metric]])))
  
  print(p)
}

# plot 3: routes
plot_list_multi_species_multi_routes <- list()
batch_count <- 1
for (sp in all_res$sp |> unique()){
  print(sp)
  sub_data <- all_res[all_res$sp==sp,]
  plot_list <- list()
  for (line in 1:nrow(sub_data)){
    this_line <- sub_data[line,]
    method <- this_line[['method']]
    model <- this_line[['model']]
    bf <- BirdFlowR::import_birdflow(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model}'))
    
    for (i in 1:8){
      p <- plot(BirdFlowR::route(bf, n=1)) + labs(title = paste0(bf$species$common_name, '; ',sp), subtitle = glue::glue("by {method}"))
      plot_list[[length(plot_list)+1]] <- p
    }
    
    p <- plot(BirdFlowR::route(bf, n=10)) + labs(title = paste0(bf$species$common_name, '; ',sp), subtitle = glue::glue("by {method}"))
    plot_list_multi_species_multi_routes[[length(plot_list_multi_species_multi_routes)+1]] <- p
  }
  
  ## plot1
  n_row <- length(sub_data$method)
  n_col <- 8
  if (!dir.exists("../../data/01.species_specific_routes/")){
    dir.create("../01.species_specific_routes/", recursive = TRUE)
  }
  pdf(glue::glue("../../data/01.species_specific_routes/{sp}.pdf"), width = 4*n_col, height = 4*n_row)  # Adjust PDF dimensions as needed
  gridExtra::grid.arrange(grobs = plot_list, nrow = n_row)  # Adjust 'ncol' to control the number of plots per row
  dev.off()
  
  n_col <- length(sub_data$method)
  
  if (length(plot_list_multi_species_multi_routes)>=6*n_col){
    ## plot2
    n_row <- 6
    pdf(glue::glue("../../data/01.species_specific_routes/multi_routes_batch_{batch_count}.pdf"), width = 6*n_col, height = 4*n_row)  # Adjust PDF dimensions as needed
    gridExtra::grid.arrange(grobs = plot_list_multi_species_multi_routes, nrow = n_row)  # Adjust 'ncol' to control the number of plots per row
    dev.off()
    
    batch_count <- batch_count+1
    plot_list_multi_species_multi_routes <- list()
  }
}


# plot 4: probability projection
lon=-75.82516
lat=1.891063
date=as.Date('2022-01-04')
plot_one_week_foward_prediction <- function(bf, lon, lat, date, world_proj){

  # latlon data for banding and encounter location
  timestep <- BirdFlowR::lookup_timestep(date, bf)
  point_df_initial <- data.frame(x = lon, y = lat)
  d_initial <- as_distr(x = point_df_initial, bf = bf, crs = 'EPSG:4326') # same as birdflow_interval_row$i1
  final_timestep <- timestep + 1
  preds <- predict(bf, d_initial, start = date, n_steps = 1)

  preds_final <- as.vector(preds[,ncol(preds),drop = FALSE]) # the projected
  
  xy <- i_to_xy(seq_along(preds[,ncol(preds)]), bf)
  xy$pred_p <- preds_final
  xy$log_pred_p <- log(xy$pred_p+1e-8)
  
  xy_start_end <- i_to_xy(c(which(preds[,1]==1)), bf) # starting location
  
  p1 <- ggplot() +
    geom_tile(data = xy, aes(x = x, y = y, fill = log_pred_p)) +
    geom_point(data = xy_start_end,
               aes(x = x, y = y),
               shape = 24,         # triangle point-up
               color = 'red',
               fill = NA,
               alpha = 0.8,
               size = 3,
               stroke = 1) +
    geom_sf(data = world_proj, fill = NA, color = "black") +
    scale_fill_viridis_c(name = "Log probability", limits = c(-15, max(xy$pred_p))) +
    coord_sf(xlim = c(min(xy$x) - 0.5*sd(xy$x), max(xy$x) + 0.5*sd(xy$x)), 
             ylim = c(min(xy$y) - 0.5*sd(xy$y), max(xy$y) + 0.5*sd(xy$y)), expand = FALSE) +
    theme_minimal()
  return(p1)
}



for (sp in all_res$sp |> unique()){
  print(sp)
  sub_data <- all_res[all_res$sp==sp,]
  plot_list <- list()
  
  ##
  # load a random model to get dates
  model <- sub_data$model[1]
  bf <- BirdFlowR::import_birdflow(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model}'))
  dates <- bf$dates$date
  
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world_proj <- st_transform(world, crs = st_crs(bf$geom$crs))
  
  for (date_ in dates){
    print(paste0(sp, ': ', date_))
    method_list <- c('LL', 'energy_score')
    all_plot_this_date <- list()
    
    for (method in method_list){
      all_plot_this_date[[method]] <- list()
      
      this_line <- sub_data[sub_data[['method']]==method,]
      model <- this_line[['model']]
      bf <- BirdFlowR::import_birdflow(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model}'))
      bf <- BirdFlowR::add_dynamic_mask(bf)
      timestep <- BirdFlowR::lookup_timestep(date_, bf)
      
      init_dist <- BirdFlowR::get_distr(bf, which=date_, from_marginals = FALSE)
      xy <- i_to_xy(seq_along(init_dist), bf)
      xy$init_p <- init_dist
      xy <- xy[bf$geom$dynamic_mask[,timestep],] # Filter by dynamic mask
      
      pos_data <- subset(xy, init_p > 1e-6)
      set.seed(which(dates==date_)) # timestep as seed
      sampled_points <- pos_data[sample(nrow(pos_data), 4), ]
      sampled_df <- BirdFlowR::xy_to_latlon(sampled_points$x, sampled_points$y, bf)
      
      for(i in 1:nrow(sampled_points)){
        sampled_df <- sampled_points[i, ]
        latlon <- BirdFlowR::xy_to_latlon(sampled_df$x, sampled_df$y, bf)
        p <- plot_one_week_foward_prediction(bf, lon = latlon$lon, lat = latlon$lat, date = date_, world_proj=world_proj)
        p <- p + labs(title = paste0(bf$species$common_name, '; ', sp),
                      subtitle = glue::glue("by {method}"))
        all_plot_this_date[[method]][[length(all_plot_this_date[[method]]) + 1]] <- p
      }
    }
    
    block1 <- do.call(gridExtra::arrangeGrob, c(all_plot_this_date[[method_list[1]]], list(ncol = 2)))
    block2 <- do.call(gridExtra::arrangeGrob, c(all_plot_this_date[[method_list[2]]], list(ncol = 2)))

    xy_ <- i_to_xy(seq_along(init_dist), bf)
    aspect_ratio <- (max(xy_$x) - min(xy_$x)) / (max(xy_$y) - min(xy_$y))
    
    if (!dir.exists(glue::glue("../../data/02.species_specific_range_projections/{sp}"))){
      dir.create(glue::glue("../../data/02.species_specific_range_projections/{sp}"), recursive = TRUE)
    }
    
    pdf(glue::glue("../../data/02.species_specific_range_projections/{sp}/{date_}.pdf"), width = 4*5, height = 4*2/aspect_ratio)  # Adjust PDF dimensions as needed
    gridExtra::grid.arrange(block1, grid::nullGrob(), block2, ncol = 3, widths = c(1, 0.2, 1))
    dev.off()
  }
}

## plot 5: real speed ranking
trait_data <- read.csv('../../data/00.sp_info/All_combined_eco_function_traits.csv')
all_res <- all_res |>
  merge(ebirdst::ebirdst_runs[,c('species_code', 'common_name')], by.x = 'sp', by.y = 'species_code', all.x=T)
tmp <- all_res[all_res$method=='energy_score',] |>
  merge(trait_data[,c('Common_Name1_eBird', 'ORDER1_eBird')], by.x = 'common_name', by.y = 'Common_Name1_eBird', all.x=T)
ggplot(data=tmp, aes(x=.data[['ORDER1_eBird']], y=.data[['synth_routes_prebreeding_migration_speed']])) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggplot(data=tmp, aes(x=.data[['ORDER1_eBird']], y=.data[['synth_routes_prebreeding_migration_n_stopovers']])) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggplot(data=tmp, aes(x=.data[['ORDER1_eBird']], y=.data[['synth_routes_prebreeding_migration_straightness']])) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


tmp_tmp <- tmp |> dplyr::arrange(-.data[['synth_routes_prebreeding_migration_speed']]) |> dplyr::sample_frac(0.1)
ggplot(data=tmp_tmp, aes(x=reorder(common_name, -synth_routes_prebreeding_migration_speed), y=synth_routes_prebreeding_migration_speed)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

###



