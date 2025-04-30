library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/03.Compare_species_specific_and_LOO/')

# devtools::install_local("/home/yc85_illinois_edu/BirdFlowR", force = T, dependencies = FALSE) # if the BirdFlowR is updated, we need to reinstall it, so that i can be used in BirdFlowPipeline!
# devtools::install_local("/home/yc85_illinois_edu/BirdFlowPipeline", force = T, dependencies = FALSE)

load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.

source('../02.Summarize_validation_preliminary/load_data_functions.R')


## load data
res <- load_raw_validation_all_sp()
raw_combined <- res[['raw_combined']]
raw_combined_with_tracking <-  res[['raw_combined_with_tracking']]

res <- load_best_models_validation_all_sp(raw_combined, raw_combined_with_tracking, include_taxomany_LOO=T)
all_res <- res[['all_res']]
all_res_with_tracking <- res[['all_res_with_tracking']]

##
all_res$base_method <- gsub("^LOO(?:_FAMILY|_ORDER)?_", "", all_res$method)
all_res$method_variation <- ifelse(grepl("^LOO_FAMILY", all_res$method), "LOO_FAMILY",
                                   ifelse(grepl("^LOO_ORDER", all_res$method), "LOO_ORDER",
                                          ifelse(grepl("^LOO_", all_res$method), "LOO", "species-specific")))

library(dplyr)

check_species_specific_better <- function(df, metric='mean_ll_improvement', against1="species-specific", against2='LOO'){
  df <- as.data.frame(df)  # convert to a plain data.frame
  if (!all(c(against1, against2) %in% df$method_variation)) {
    return(NA_real_)
  }
  species_val <- df[df$method_variation == against1, metric, drop = TRUE]
  loo_val <- df[df$method_variation == against2, metric, drop = TRUE]
  better <- species_val - loo_val
  return(better)
}

compared_df <- all_res |>
  dplyr::group_by(sp, base_method) |>
  dplyr::summarize(
    species_specific_better_by_LL = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_ll_improvement'),
    species_specific_better_by_distance_metric = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_win_distance'),
    species_specific_better_by_energy_score = check_species_specific_better(dplyr::cur_data(), metric='mean_win_distance_fraction'),
    training_n_intervals = .data[['training_n_intervals']][1]
  ) |>
  na.omit()

trait_data <- read.csv('../../data/00.sp_info/All_combined_eco_function_traits.csv')
compared_df <- compared_df |>
  merge(ebirdst::ebirdst_runs[,c('species_code', 'common_name')], by.x = 'sp', by.y = 'species_code', all.x=T)

tmp <- compared_df[compared_df$base_method=='LL',]
library(ggplot2)
library(ggrepel)
ggplot(tmp, aes(x = training_n_intervals, y = species_specific_better_by_LL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 20) +
  labs(x = "Training Intervals", y = "How much does species-specific model improves the LL\ncompared to LOO")

ggplot(tmp, aes(x = training_n_intervals, y = species_specific_better_by_distance_metric)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 20) +
  labs(x = "Training Intervals", y = "How much does species-specific model improves the distance metric\ncompared to LOO")

ggplot(tmp, aes(x = training_n_intervals, y = species_specific_better_by_energy_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 10) +
  labs(x = "Training Intervals", y = "How much does species-specific model improves the energy score\ncompared to LOO")

##
ggplot(tmp, aes(x = species_specific_better_by_LL, y = species_specific_better_by_distance_metric)) +
  geom_point(aes(color=training_n_intervals)) +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 30) +
  labs(x = "How much does species-specific model improves the LL\ncompared to LOO", 
       y = "How much does species-specific model improves the distance metric\ncompared to LOO") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") +
  scale_color_viridis_c()

## LOO order to LOO all
compared_df <- all_res |>
  dplyr::group_by(sp, base_method) |>
  dplyr::summarize(
    species_specific_better_by_LL = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_ll_improvement', against1="LOO_ORDER", against2='LOO'),
    species_specific_better_by_distance_metric = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_win_distance', against1="LOO_ORDER", against2='LOO'),
    species_specific_better_by_energy_score = check_species_specific_better(dplyr::cur_data(), metric='mean_win_distance_fraction', against1="LOO_ORDER", against2='LOO'),
    training_n_intervals = .data[['training_n_intervals']][1]
  ) |>
  na.omit()
trait_data <- read.csv('../../data/00.sp_info/All_combined_eco_function_traits.csv')
compared_df <- compared_df |>
  merge(ebirdst::ebirdst_runs[,c('species_code', 'common_name')], by.x = 'sp', by.y = 'species_code', all.x=T)

tmp <- compared_df[compared_df$base_method=='LL',]

ggplot(tmp, aes(x = species_specific_better_by_LL, y = species_specific_better_by_distance_metric)) +
  geom_point(aes(color=training_n_intervals)) +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 30) +
  labs(x = "How much does LOO_ORDER model improves the LL\ncompared to LOO", 
       y = "How much does LOO_ORDER model improves the distance metric\ncompared to LOO") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") +
  scale_color_viridis_c()


## LOO family to LOO all
compared_df <- all_res |>
  dplyr::group_by(sp, base_method) |>
  dplyr::summarize(
    species_specific_better_by_LL = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_ll_improvement', against1="LOO_FAMILY", against2='LOO'),
    species_specific_better_by_distance_metric = check_species_specific_better(dplyr::cur_data(), metric='weighted_mean_win_distance', against1="LOO_FAMILY", against2='LOO'),
    species_specific_better_by_energy_score = check_species_specific_better(dplyr::cur_data(), metric='mean_win_distance_fraction', against1="LOO_FAMILY", against2='LOO'),
    training_n_intervals = .data[['training_n_intervals']][1]
  ) |>
  na.omit()
trait_data <- read.csv('../../data/00.sp_info/All_combined_eco_function_traits.csv')
compared_df <- compared_df |>
  merge(ebirdst::ebirdst_runs[,c('species_code', 'common_name')], by.x = 'sp', by.y = 'species_code', all.x=T)

tmp <- compared_df[compared_df$base_method=='LL',]

ggplot(tmp, aes(x = species_specific_better_by_LL, y = species_specific_better_by_distance_metric)) +
  geom_point(aes(color=training_n_intervals)) +
  geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 30) +
  labs(x = "How much does LOO_FAMILY model improves the LL\ncompared to LOO", 
       y = "How much does LOO_FAMILY model improves the distance metric\ncompared to LOO") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") +
  scale_color_viridis_c()






