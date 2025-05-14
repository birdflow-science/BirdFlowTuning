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
all_res <- all_res |> dplyr::group_by(.data[['sp']], .data[['method']]) |> dplyr::slice(1) |> dplyr::ungroup()

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

## plot 7: Three lines together, using LL
# 1. x: all-LOO; y: Species-specific
# 2. x: all-LOO; y: Family-LOO
# 3. x: all-LOO; y: Order-LOO
all_res <- all_res[all_res$base_method=="ST098_and_LL",]
all_res$method

all_res_long <- all_res[,c('sp', 'method_variation', 'weighted_mean_ll_improvement')] |> 
  dplyr::group_by(.data[['sp']], .data[['method_variation']]) |> 
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    id_cols = sp,
    names_from = method_variation,
    values_from = weighted_mean_ll_improvement
  )
is_na <- is.na(all_res_long$LOO_FAMILY)
all_res_long$LOO_FAMILY <- ifelse(is.na(all_res_long$LOO_FAMILY), all_res_long[['species-specific']], all_res_long$LOO_FAMILY)
all_res_long$LOO_ORDER <- ifelse(is.na(all_res_long$LOO_ORDER), all_res_long[['species-specific']], all_res_long$LOO_ORDER)

p <- ggplot(data=all_res_long) + 
  geom_point(aes(x=.data[['LOO']], y=.data[['species-specific']]),
             shape = 21, size=3, alpha=0.7,
             fill = "steelblue",
             color = "black",
             stroke = 0.5) + 
  geom_smooth(method='lm', aes(x=.data[['LOO']], y=.data[['species-specific']], color = "Species-specific"),
              fill='steelblue', alpha=0.2) +
  geom_point(aes(x=.data[['LOO']], y=.data[['LOO_FAMILY']]),
             shape = 21, size=3, alpha=0.7,
             fill = "orange2",
             color = "black",
             stroke = 0.5) + 
  geom_smooth(method='lm', aes(x=.data[['LOO']], y=.data[['LOO_FAMILY']], color = "Family-LOO"),
              fill='orange2', alpha=0.2) +
  geom_point(aes(x=.data[['LOO']], y=.data[['LOO_ORDER']]),
             shape = 21, size=3, alpha=0.7,
             fill = "green3",
             color = "black",
             stroke = 0.5) + 
  geom_smooth(method='lm', aes(x=.data[['LOO']], y=.data[['LOO_ORDER']], color = "Order-LOO"),
              fill='green3', alpha=0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red2") +
  scale_color_manual(
    name   = "Method",
    values = c(
      "Species-specific" = "steelblue",
      "Family-LOO"  = "orange2",
      "Order-LOO"  = "green3"
    )
  ) +
  labs(x='Log likelihood for All-LOO (baseline)', y='Log likelihood') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]
  

cairo_pdf('../../data/06.Compare_species_specific_and_LOO/07.All_three_methods_LL.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()


## plot 8: Three lines together, using Relative distance gain
all_res_long <- all_res[,c('sp', 'method_variation', 'weighted_mean_win_distance_fraction')] |> 
  dplyr::group_by(.data[['sp']], .data[['method_variation']]) |> 
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    id_cols = sp,
    names_from = method_variation,
    values_from = weighted_mean_win_distance_fraction
  )
is_na <- is.na(all_res_long$LOO_FAMILY)
all_res_long$LOO_FAMILY <- ifelse(is.na(all_res_long$LOO_FAMILY), all_res_long[['species-specific']], all_res_long$LOO_FAMILY)
all_res_long$LOO_ORDER <- ifelse(is.na(all_res_long$LOO_ORDER), all_res_long[['species-specific']], all_res_long$LOO_ORDER)

p <- ggplot(data=all_res_long) + 
  geom_point(aes(x=.data[['LOO']], y=.data[['species-specific']]),
             shape = 21, size=3, alpha=0.7,
             fill = "steelblue",
             color = "black",
             stroke = 0.5) + 
  geom_smooth(method='lm', aes(x=.data[['LOO']], y=.data[['species-specific']], color = "Species-specific"),
              fill='steelblue', alpha=0.2) +
  geom_point(aes(x=.data[['LOO']], y=.data[['LOO_FAMILY']]),
             shape = 21, size=3, alpha=0.7,
             fill = "orange2",
             color = "black",
             stroke = 0.5) + 
  geom_smooth(method='lm', aes(x=.data[['LOO']], y=.data[['LOO_FAMILY']], color = "Family-LOO"),
              fill='orange2', alpha=0.2) +
  geom_point(aes(x=.data[['LOO']], y=.data[['LOO_ORDER']]),
             shape = 21, size=3, alpha=0.7,
             fill = "green3",
             color = "black",
             stroke = 0.5) + 
  geom_smooth(method='lm', aes(x=.data[['LOO']], y=.data[['LOO_ORDER']], color = "Order-LOO"),
              fill='green3', alpha=0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red2") +
  scale_color_manual(
    name   = "Method",
    values = c(
      "Species-specific" = "steelblue",
      "Family-LOO"  = "orange2",
      "Order-LOO" = "green3"
    )
  ) +
  labs(x='Relative distance gain\nfor All-LOO (baseline)', y='Relative distance gain') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]


cairo_pdf('../../data/06.Compare_species_specific_and_LOO/08.All_three_methods_Relative_distance_gain.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()


## plot 9: Three lines together, using distance gain
all_res_long <- all_res[,c('sp', 'method_variation', 'weighted_mean_win_distance')] |> 
  dplyr::group_by(.data[['sp']], .data[['method_variation']]) |> 
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    id_cols = sp,
    names_from = method_variation,
    values_from = weighted_mean_win_distance
  )
is_na <- is.na(all_res_long$LOO_FAMILY)
all_res_long$LOO_FAMILY <- ifelse(is.na(all_res_long$LOO_FAMILY), all_res_long[['species-specific']], all_res_long$LOO_FAMILY)
all_res_long$LOO_ORDER <- ifelse(is.na(all_res_long$LOO_ORDER), all_res_long[['species-specific']], all_res_long$LOO_ORDER)

p <- ggplot(data=all_res_long) + 
  geom_point(aes(x=.data[['LOO']], y=.data[['species-specific']]),
             shape = 21, size=3, alpha=0.7,
             fill = "steelblue",
             color = "black",
             stroke = 0.5) + 
  geom_smooth(method='lm', aes(x=.data[['LOO']], y=.data[['species-specific']], color = "Species-specific"),
              fill='steelblue', alpha=0.2) +
  geom_point(aes(x=.data[['LOO']], y=.data[['LOO_FAMILY']]),
             shape = 21, size=3, alpha=0.7,
             fill = "orange2",
             color = "black",
             stroke = 0.5) + 
  geom_smooth(method='lm', aes(x=.data[['LOO']], y=.data[['LOO_FAMILY']], color = "Family-LOO"),
              fill='orange2', alpha=0.2) +
  geom_point(aes(x=.data[['LOO']], y=.data[['LOO_ORDER']]),
             shape = 21, size=3, alpha=0.7,
             fill = "green3",
             color = "black",
             stroke = 0.5) + 
  geom_smooth(method='lm', aes(x=.data[['LOO']], y=.data[['LOO_ORDER']], color = "Order-LOO"),
              fill='green3', alpha=0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red2") +
  scale_color_manual(
    name   = "Method",
    values = c(
      "Species-specific" = "steelblue",
      "Family-LOO"  = "orange2",
      "Order-LOO"= "green3"
    )
  ) +
  labs(x='Distance gain (km)\nfor All-LOO (baseline)', y='Distance gain (km)') +
  scale_x_log10() +
  scale_y_log10() +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]


cairo_pdf('../../data/06.Compare_species_specific_and_LOO/09.All_three_methods_Distance_gain.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()

## plot 10: Three together, but histogram, using LL
all_res_long <- all_res[,c('sp', 'method_variation', 'weighted_mean_ll_improvement')] |> 
  dplyr::group_by(.data[['sp']], .data[['method_variation']]) |> 
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    id_cols = sp,
    names_from = method_variation,
    values_from = weighted_mean_ll_improvement
  )
is_na <- is.na(all_res_long$LOO_FAMILY)
all_res_long$LOO_FAMILY <- ifelse(is.na(all_res_long$LOO_FAMILY), all_res_long[['species-specific']], all_res_long$LOO_FAMILY)
all_res_long$LOO_ORDER <- ifelse(is.na(all_res_long$LOO_ORDER), all_res_long[['species-specific']], all_res_long$LOO_ORDER)

# subtract
all_res_long$LOO_FAMILY <- all_res_long$LOO_FAMILY - all_res_long$LOO
all_res_long$LOO_ORDER <- all_res_long$LOO_ORDER - all_res_long$LOO
all_res_long$`species-specific` <- all_res_long$`species-specific` - all_res_long$LOO
all_res_long$`Species-specific` <- all_res_long$`species-specific`
all_res_long$`Family-LOO` <- all_res_long$`LOO_FAMILY`
all_res_long$`Order-LOO` <- all_res_long$`LOO_ORDER`

df_long <- all_res_long |>
  select(`Species-specific`, `Family-LOO`, `Order-LOO`) |>
  pivot_longer(
    cols = everything(),
    names_to = "method",
    values_to = "LOO"
  )
df_long$method <- factor(df_long$method, levels = c("Order-LOO", "Family-LOO", "Species-specific"))
means <- df_long |>
  group_by(method) |>
  summarise(median_val = median(LOO, na.rm = TRUE),
            mean_val = mean(LOO, na.rm = TRUE))

p <- ggplot(df_long, aes(x = LOO, fill = method)) +
  geom_histogram(bins = 30, color = "black") +
  geom_vline(data = means, aes(xintercept = mean_val),
             color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ method, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "Species-specific" = "steelblue",
    "Family-LOO"       = "orange2",
    "Order-LOO"        = "green3"
  )) +
  theme(
    panel.background = element_blank(),    # no gray
    panel.grid       = element_blank(),    # no grid
    axis.line        = element_blank(),    # remove default axes
    panel.border     = element_rect(       # add a black border
      colour = "black", 
      fill   = NA, 
      linewidth   = 1
    ),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),
    legend.position = "none"
  ) + 
  labs(x='Log likelihood improvement over All-LOO', y='Species count') +
  ggh4x::force_panelsizes(rows = unit(2, "in"),
                              cols = unit(5, "in"))

cairo_pdf('../../data/06.Compare_species_specific_and_LOO/10.All_three_methods_LL_hist.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()


## plot 11: Three together, but histogram, using relative distance gain
all_res_long <- all_res[,c('sp', 'method_variation', 'weighted_mean_win_distance_fraction')] |> 
  dplyr::group_by(.data[['sp']], .data[['method_variation']]) |> 
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    id_cols = sp,
    names_from = method_variation,
    values_from = weighted_mean_win_distance_fraction
  )
is_na <- is.na(all_res_long$LOO_FAMILY)
all_res_long$LOO_FAMILY <- ifelse(is.na(all_res_long$LOO_FAMILY), all_res_long[['species-specific']], all_res_long$LOO_FAMILY)
all_res_long$LOO_ORDER <- ifelse(is.na(all_res_long$LOO_ORDER), all_res_long[['species-specific']], all_res_long$LOO_ORDER)

# subtract
all_res_long$LOO_FAMILY <- all_res_long$LOO_FAMILY - all_res_long$LOO
all_res_long$LOO_ORDER <- all_res_long$LOO_ORDER - all_res_long$LOO
all_res_long$`species-specific` <- all_res_long$`species-specific` - all_res_long$LOO
all_res_long$`Species-specific` <- all_res_long$`species-specific`
all_res_long$`Family-LOO` <- all_res_long$`LOO_FAMILY`
all_res_long$`Order-LOO` <- all_res_long$`LOO_ORDER`

df_long <- all_res_long |>
  select(`Species-specific`, `Family-LOO`, `Order-LOO`) |>
  pivot_longer(
    cols = everything(),
    names_to = "method",
    values_to = "LOO"
  )
df_long$method <- factor(df_long$method, levels = c("Order-LOO", "Family-LOO", "Species-specific"))
means <- df_long |>
  group_by(method) |>
  summarise(median_val = median(LOO, na.rm = TRUE),
            mean_val = mean(LOO, na.rm = TRUE))

p <- ggplot(df_long, aes(x = LOO, fill = method)) +
  geom_histogram(bins = 30, color = "black") +
  geom_vline(data = means, aes(xintercept = mean_val),
             color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ method, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "Species-specific" = "steelblue",
    "Family-LOO"       = "orange2",
    "Order-LOO"        = "green3"
  )) +
  theme(
    panel.background = element_blank(),    # no gray
    panel.grid       = element_blank(),    # no grid
    axis.line        = element_blank(),    # remove default axes
    panel.border     = element_rect(       # add a black border
      colour = "black", 
      fill   = NA, 
      linewidth   = 1
    ),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),
    legend.position = "none"
  ) + 
  labs(x='Relative distance gain improvement\nover All-LOO', y='Species count') +
  ggh4x::force_panelsizes(rows = unit(2, "in"),
                          cols = unit(5, "in"))

cairo_pdf('../../data/06.Compare_species_specific_and_LOO/11.All_three_methods_Relative_distance_gain_hist.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()


## plot 12: Three together, but histogram, using distance gain
all_res_long <- all_res[,c('sp', 'method_variation', 'weighted_mean_win_distance')] |> 
  dplyr::group_by(.data[['sp']], .data[['method_variation']]) |> 
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    id_cols = sp,
    names_from = method_variation,
    values_from = weighted_mean_win_distance
  )
is_na <- is.na(all_res_long$LOO_FAMILY)
all_res_long$LOO_FAMILY <- ifelse(is.na(all_res_long$LOO_FAMILY), all_res_long[['species-specific']], all_res_long$LOO_FAMILY)
all_res_long$LOO_ORDER <- ifelse(is.na(all_res_long$LOO_ORDER), all_res_long[['species-specific']], all_res_long$LOO_ORDER)

# subtract
all_res_long$LOO_FAMILY <- all_res_long$LOO_FAMILY - all_res_long$LOO
all_res_long$LOO_ORDER <- all_res_long$LOO_ORDER - all_res_long$LOO
all_res_long$`species-specific` <- all_res_long$`species-specific` - all_res_long$LOO
all_res_long$`Species-specific` <- all_res_long$`species-specific`
all_res_long$`Family-LOO` <- all_res_long$`LOO_FAMILY`
all_res_long$`Order-LOO` <- all_res_long$`LOO_ORDER`

df_long <- all_res_long |>
  select(`Species-specific`, `Family-LOO`, `Order-LOO`) |>
  pivot_longer(
    cols = everything(),
    names_to = "method",
    values_to = "LOO"
  )
df_long$method <- factor(df_long$method, levels = c("Order-LOO", "Family-LOO", "Species-specific"))
means <- df_long |>
  group_by(method) |>
  summarise(median_val = median(LOO, na.rm = TRUE),
            mean_val = mean(LOO, na.rm = TRUE))

p <- ggplot(df_long, aes(x = LOO, fill = method)) +
  geom_histogram(bins = 30, color = "black") +
  geom_vline(data = means, aes(xintercept = mean_val),
             color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ method, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "Species-specific" = "steelblue",
    "Family-LOO"       = "orange2",
    "Order-LOO"        = "green3"
  )) +
  theme(
    panel.background = element_blank(),    # no gray
    panel.grid       = element_blank(),    # no grid
    axis.line        = element_blank(),    # remove default axes
    panel.border     = element_rect(       # add a black border
      colour = "black", 
      fill   = NA, 
      linewidth   = 1
    ),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),
    legend.position = "none"
  ) + 
  labs(x='Distance gain improvement\nover All-LOO (km)', y='Species count') +
  ggh4x::force_panelsizes(rows = unit(2, "in"),
                          cols = unit(5, "in"))

cairo_pdf('../../data/06.Compare_species_specific_and_LOO/12.All_three_methods_distance_gain_hist.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()


