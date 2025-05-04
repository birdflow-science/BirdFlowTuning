library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)
library(dplyr)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/05.Compare_species_specific_and_LOO/')

load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.

source('../plotting_params/plotting_params.R')
# source('../02.Summarize_validation_preliminary/load_data_functions.R')


## 01. Load data
all_res <- read.csv('../../data/03.All_validation_summary/validation_final_summary_filtered.csv')

##
all_res$base_method <- gsub("^LOO(?:_FAMILY|_ORDER)?_", "", all_res$method)
all_res$method_variation <- ifelse(grepl("^LOO_FAMILY", all_res$method), "LOO_FAMILY",
                                   ifelse(grepl("^LOO_ORDER", all_res$method), "LOO_ORDER",
                                          ifelse(grepl("^LOO_", all_res$method), "LOO", "species-specific")))

check_species_specific_better <- function(df, metric='ST098_and_LL', against1="species-specific", against2='LOO'){
  df <- as.data.frame(df)  # convert to a plain data.frame
  if (!all(c(against1, against2) %in% df$method_variation)) {
    return(NA_real_)
  }
  species_val <- df[df$method_variation == against1, metric, drop = TRUE][1]
  loo_val <- df[df$method_variation == against2, metric, drop = TRUE][1]
  better <- species_val - loo_val

  return(better)
}

## 02. Compare different LOO methods
compared_df <- all_res |>
  dplyr::group_by(sp, base_method) |>
  dplyr::summarize(
    species_specific_better_by_LL = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_ll_improvement'),
    species_specific_better_by_distance_metric = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_win_distance_fraction'),
    species_specific_better_by_energy_score = check_species_specific_better(dplyr::cur_data(), metric='weighted_energy_improvement'),
    training_n_intervals = .data[['training_n_intervals']][1]
  ) |>
  na.omit()

trait_data <- read.csv('../../data/00.sp_info/All_combined_eco_function_traits.csv')
compared_df <- compared_df |>
  merge(ebirdst::ebirdst_runs[,c('species_code', 'common_name')], by.x = 'sp', by.y = 'species_code', all.x=T)

tmp <- compared_df[compared_df$base_method=='ST098_and_LL',]

## 03. Plot
library(ggplot2)
library(ggrepel)

## plot 1
p <- ggplot(tmp, aes(x = training_n_intervals, y = species_specific_better_by_LL)) +
  my_plotting_params[['scatter']] +
  geom_smooth(method = "lm", se = TRUE, color='steelblue') +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 10) +
  labs(x = "Training Intervals", y = "LL advantages for species-specific\nmodel over LOO model") +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

cairo_pdf('../../data/06.Compare_species_specific_and_LOO/01.LL_advantages_over_LOO.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()

## plot 2
p <- ggplot(tmp, aes(x = training_n_intervals, y = species_specific_better_by_distance_metric)) +
  my_plotting_params[['scatter']] +
  geom_smooth(method = "lm", se = TRUE, color='steelblue') +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 10) +
  labs(x = "Training Intervals", y = "Relative distance gain advantages for\nspecies-specific model over LOO model") +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

cairo_pdf('../../data/06.Compare_species_specific_and_LOO/02.relative_distance_gain_advantages_over_LOO.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()

## plot 3
p <- ggplot(tmp, aes(x = training_n_intervals, y = species_specific_better_by_energy_score)) +
  my_plotting_params[['scatter']] +
  geom_smooth(method = "lm", se = TRUE, color='steelblue') +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 10) +
  labs(x = "Training Intervals", y = "Energy score advantages for\nspecies-specific model over LOO model") +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

cairo_pdf('../../data/06.Compare_species_specific_and_LOO/03.energy_score_advantages_over_LOO.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()


## plot 4: LOO to species-specific
p <- ggplot(tmp, aes(x = species_specific_better_by_LL, y = species_specific_better_by_distance_metric)) +
  my_plotting_params[['scatter']] +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 5) +
  labs(x = "LL advantages for species-specific\nmodel over all-LOO model", 
       y = "Relative distance gain advantages for\nspecies-specific model over all-LOO model") +
  my_plotting_params[['zero_vline']] +
  my_plotting_params[['zero_hline']] +
  scale_color_viridis_c() +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

cairo_pdf('../../data/06.Compare_species_specific_and_LOO/04.LL_advantages_and_distance_advantages_compared_to_LOO.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()


## plot 5: LOO order to LOO all
compared_df <- all_res |>
  dplyr::group_by(sp, base_method) |>
  dplyr::summarize(
    species_specific_better_by_LL = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_ll_improvement', against1="LOO_ORDER", against2='LOO'),
    species_specific_better_by_distance_metric = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_win_distance_fraction', against1="LOO_ORDER", against2='LOO'),
    species_specific_better_by_energy_score = check_species_specific_better(dplyr::cur_data(), metric='weighted_energy_improvement', against1="LOO_ORDER", against2='LOO'),
    training_n_intervals = .data[['training_n_intervals']][1]
  ) |>
  na.omit()

compared_df <- compared_df |>
  merge(ebirdst::ebirdst_runs[,c('species_code', 'common_name')], by.x = 'sp', by.y = 'species_code', all.x=T)

tmp <- compared_df[compared_df$base_method=='ST098_and_LL',]

p <- ggplot(tmp, aes(x = species_specific_better_by_LL, y = species_specific_better_by_distance_metric)) +
  my_plotting_params[['scatter']] +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 5) +
  labs(x = "LL advantages for order-LOO\nmodel over all-LOO model", 
       y = "Relative distance gain advantages for\norder-LOO model over all-LOO model") +
  my_plotting_params[['zero_vline']] +
  my_plotting_params[['zero_hline']] +
  scale_color_viridis_c() +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

cairo_pdf('../../data/06.Compare_species_specific_and_LOO/05.LL_advantages_and_distance_advantages_compared_to_LOO_order.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()


## plot 6: LOO family to LOO all
compared_df <- all_res |>
  dplyr::group_by(sp, base_method) |>
  dplyr::summarize(
    species_specific_better_by_LL = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_ll_improvement', against1="LOO_FAMILY", against2='LOO'),
    species_specific_better_by_distance_metric = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_win_distance_fraction', against1="LOO_FAMILY", against2='LOO'),
    species_specific_better_by_energy_score = check_species_specific_better(dplyr::cur_data(), metric='weighted_energy_improvement', against1="LOO_FAMILY", against2='LOO'),
    training_n_intervals = .data[['training_n_intervals']][1]
  ) |>
  na.omit()

compared_df <- compared_df |>
  merge(ebirdst::ebirdst_runs[,c('species_code', 'common_name')], by.x = 'sp', by.y = 'species_code', all.x=T)

tmp <- compared_df[compared_df$base_method=='ST098_and_LL',]

p <- ggplot(tmp, aes(x = species_specific_better_by_LL, y = species_specific_better_by_distance_metric)) +
  my_plotting_params[['scatter']] +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 5) +
  labs(x = "LL advantages for family-LOO\nmodel over all-LOO model", 
       y = "Relative distance gain advantages for\nfamily-LOO model over all-LOO model") +
  my_plotting_params[['zero_vline']] +
  my_plotting_params[['zero_hline']] +
  scale_color_viridis_c() +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

cairo_pdf('../../data/06.Compare_species_specific_and_LOO/06.LL_advantages_and_distance_advantages_compared_to_LOO_family.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()






