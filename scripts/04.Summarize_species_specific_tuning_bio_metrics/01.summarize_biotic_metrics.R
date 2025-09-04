library(ggplot2)
library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)
library(dplyr)
library(tidyr)
library(scales)
library(ggh4x)
library(grid)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/04.Summarize_species_specific_tuning_bio_metrics/')
source('../plotting_params/plotting_params.R')
source('../02.Summarize_validation_preliminary/load_data_functions.R')

all_res_with_tracking <- read.csv('../../data/03.All_validation_summary/validation_compare_methods_tracking_sp_only_summary.csv')

## 01. Do conditional route stats
tmp <- all_res_with_tracking[all_res_with_tracking['method']=='ST098_and_LL',] ## ST098_and_LL as model selection method
tmp <- tmp |> dplyr::group_by(.data[['sp']]) |> dplyr::slice(1) |> dplyr::ungroup()

all_conditional_rts_stats_res <- list()
MSE <- list()
# sims_long <- list()
# real_long <- list()

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
  real_track$data$route_id <- paste0(real_track$data$route_id, "_prebreeding")
  
  params$season <- 'postbreeding'
  real_track2 <- get_real_track(bf, params, filter=TRUE) #
  real_track2$data$route_id <- paste0(real_track2$data$route_id, "_postbreeding")
  
  real_track$data <- rbind(real_track$data, real_track2$data)
  
  if (is.null(real_track)) {
    print(paste0('No tracking data: ', sp))
    next
  }
  
  splitted_track <- split(real_track$data, real_track$data$route_id)
  all_route_stats <- list()
  for (i in seq_along(splitted_track)) {
    cat(sprintf("\rProcessing track %d/%d", i, length(splitted_track)))
    flush.console()
    track <- splitted_track[[i]]
    cur_track_id <- names(splitted_track)[i]
    
    track_duration <- as.integer(max(track$date) - min(track$date))
    track_log_frequency <- nrow(track) / track_duration
    if ((track_log_frequency < 1/7) | track_duration < 14){
      next # should have enough intermediate logs for behavior inference
    }
    
    n_sim_tracks <- 100
    the_synth_track <- BirdFlowR::route(bf = bf, n = n_sim_tracks, 
                                        start = track$timestep[1], end=track$timestep[nrow(track)], 
                                        x_coord = track$x[1], y_coord = track$y[1], 
                                        from_marginals = TRUE)
    
    all_route_synth <- list()
    for (n_i in 1:n_sim_tracks) {
      tmp_synth_track <- the_synth_track
      tmp_synth_track$data <- tmp_synth_track$data[tmp_synth_track$data$route_id==n_i, ]
      tmp_route_stats_synth <- rts_stats(tmp_synth_track)
      tmp_route_stats_synth <- as.data.frame(tmp_route_stats_synth)
      names(tmp_route_stats_synth) <- paste0(names(tmp_route_stats_synth), "_synth")
      all_route_synth[[n_i]] <- tmp_route_stats_synth
    }
    all_route_synth <- do.call(rbind, all_route_synth)

    quantiles <- sapply(all_route_synth, function(x) {
      quantile(x, probs = c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
    })
    a <- quantiles[1,]
    names(a) <- paste0(names(a), '_0025_quantile')
    b <- quantiles[2,]
    names(b) <- paste0(names(b), '_0250_quantile')
    c <- quantiles[3,]
    names(c) <- paste0(names(c), '_0750_quantile')
    d <- quantiles[4,]
    names(d) <- paste0(names(d), '_0975_quantile')
    
    all_route_synth <- cbind(t(as.data.frame(a)), t(as.data.frame(b)),
                             t(as.data.frame(c)), t(as.data.frame(d)))
    
    route_stats_real <- rts_stats(list(data=track))
    route_stats_real <- as.data.frame(route_stats_real)

    names(route_stats_real) <- paste0(names(route_stats_real), "_real")

    all_route_stats[[length(all_route_stats) + 1]] <- cbind(all_route_synth, route_stats_real)
  }
  
  conditional_rts_stats_res <- as.data.frame(do.call(rbind, all_route_stats))
  conditional_rts_stats_res$sp <- sp
  all_conditional_rts_stats_res[[length(all_conditional_rts_stats_res) + 1]] <- conditional_rts_stats_res

  mse_straightness <- mean(na.omit((conditional_rts_stats_res$straightness_real - conditional_rts_stats_res$straightness_synth))^2)
  mse_n_stopovers <- mean(na.omit((conditional_rts_stats_res$n_stopovers_real - conditional_rts_stats_res$n_stopovers_synth))^2)
  mse_speed <- mean(na.omit((conditional_rts_stats_res$speed_real - conditional_rts_stats_res$speed_synth))^2)
  
  MSE[[length(MSE) + 1]] <- list(sp=sp, mse_straightness=mse_straightness, mse_n_stopovers=mse_n_stopovers, mse_speed=mse_speed)
}



## 1) Bind results from all species
df_all <- dplyr::bind_rows(all_conditional_rts_stats_res)

## 2) Keep just what we need and add a per-track id (within species)
keep_cols <- c(
  "sp",
  # 95%
  "straightness_synth_0025_quantile","straightness_synth_0975_quantile",
  "speed_synth_0025_quantile","speed_synth_0975_quantile",
  "n_stopovers_synth_0025_quantile","n_stopovers_synth_0975_quantile",
  # 50%
  "straightness_synth_0250_quantile","straightness_synth_0750_quantile",
  "speed_synth_0250_quantile","speed_synth_0750_quantile",
  "n_stopovers_synth_0250_quantile","n_stopovers_synth_0750_quantile",
  # real
  "straightness_real","speed_real","n_stopovers_real"
)

df_wide <- df_all[, intersect(keep_cols, names(df_all))] |>
  group_by(sp) |>
  mutate(track_id = row_number()) |>   # <-- unique per species
  ungroup()

## 3) Long reshape with the track id preserved
df_long <- df_wide |>
  pivot_longer(
    cols = -c(sp, track_id),
    names_to = "name",
    values_to = "value"
  ) |>
  mutate(
    metric = case_when(
      grepl("^straightness", name) ~ "straightness",
      grepl("^speed", name) ~ "speed",
      grepl("^n_stopovers",  name) ~ "n_stopovers",
      TRUE ~ NA_character_
    ),
    stat = case_when(
      grepl("_0025_quantile$", name) ~ "q0025",
      grepl("_0250_quantile$", name) ~ "q0250",
      grepl("_0750_quantile$", name) ~ "q0750",
      grepl("_0975_quantile$", name) ~ "q0975",
      grepl("_real$", name) ~ "real",
      TRUE ~ NA_character_
    )
  ) |>
  select(sp, track_id, metric, stat, value) |>
  pivot_wider(
    id_cols = c(sp, track_id, metric),
    names_from = stat,
    values_from = value
  )

## 4) Coerce quantiles/real to numeric (now they’re vectors, not lists)
df_long <- df_long |>
  mutate(across(c(q0025, q0250, q0750, q0975, real), ~ suppressWarnings(as.numeric(.))))

## 5) Drop rows without an empirical value; order within species × metric
df_long <- df_long |>
  filter(!is.na(real)) |>
  group_by(sp, metric) |>
  mutate(
    mid95 = (q0025 + q0975) / 2,
    row_in_panel = factor(order(order(mid95)))  # ordered y for each facet
  ) |>
  ungroup()


set.seed(42)
per_panel_n <- 30  # random sample

# Order species and metrics (3 rows)
sp_levels     <- sort(unique(df_long$sp))
metric_levels <- c("straightness", "n_stopovers", "speed")
metric_labels <- c(
  straightness = "Route straightness",
  n_stopovers  = "# Stopovers",
  speed        = "Speed (m/day)"
)

# Build plotting dataframe
df_plot <- df_long |>
  filter(!is.na(real), !is.na(q0250), !is.na(q0750)) |>
  mutate(
    sp     = factor(sp, levels = sp_levels),
    metric = factor(metric, levels = metric_levels),
    mid50  = (q0250 + q0750) / 2
  ) |>
  group_by(sp, metric) |>
  group_modify(~ {
    n_take <- if (is.na(per_panel_n)) nrow(.x) else min(per_panel_n, nrow(.x))
    .x[sample.int(nrow(.x), n_take), , drop = FALSE]
  }) |>
  arrange(mid50, .by_group = TRUE) |>
  mutate(y_rank = dplyr::row_number()) |>
  ungroup()

breaks_inset_3 <- function(lims, inset = 0.07) {
  rng <- range(lims, finite = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) == 0) return(rng[1])
  a <- rng[1] + inset * diff(rng)
  b <- rng[2] - inset * diff(rng)
  seq(a, b, length.out = 3)
}


####### 01. horizontal distribution plot
# Base plot
p <- ggplot(df_plot, aes(y = y_rank)) +
  geom_errorbarh(aes(xmin = q0025, xmax = q0975),
                 height = 0, color = "grey55") +
  geom_errorbarh(aes(xmin = q0250, xmax = q0750),
                 height = 0, linewidth = 1.2, color = "grey35") +
  geom_point(aes(x = real), color = "red", size = 1.6, alpha = 0.7) +
  labs(
    x = NULL, y = NULL,
    title    = "Observed metrics (red) vs. simulated metrics per track (gray)",
    subtitle = "Thin = 95% (2.5–97.5%); Thick = 50% (25–75%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.placement    = "outside",
    strip.background   = element_blank()
  ) +
  ggh4x::facet_grid2(
    rows  = vars(metric),
    cols  = vars(sp),
    scales = "free",          # free both axes
    independent = "all",      # ← FIX: make x and y independent per panel
    labeller = labeller(metric = metric_labels)
  ) +
  scale_y_continuous(breaks = NULL, expand = expansion(add = 0.5))

# 2) make columns a bit farther apart + allow labels to render outside panels
p <- p +
  theme(
    panel.spacing.x = unit(0.4, "cm"),
    panel.spacing.y = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8, margin = margin(t = 1))
  ) +
  coord_cartesian(clip = "off")  # prevents first speed label from being clipped

# 3) per-row x scales: keep straightness/stopovers as-is, adjust speed
p <- p +
  ggh4x::facetted_pos_scales(
    x = list(
      metric == "straightness" ~ scale_x_continuous(
        breaks = breaks_inset_3,
        labels = scales::label_number(accuracy = 0.1),
        expand = expansion(mult = c(0.02, 0.02), add = 0.1)
      ),
      metric == "n_stopovers" ~ scale_x_continuous(
        breaks = scales::breaks_pretty(n = 3),
        labels = scales::label_number(accuracy = 0.1),
        expand = expansion(mult = c(0.02, 0.02), add = 0.1)
      ),
      # SPEED: inset breaks (no border ticks) + extra expansion + scientific labels
      metric == "speed" ~ scale_x_continuous(
        breaks = breaks_inset_3,
        labels = scales::label_scientific(digits = 1),
        expand = expansion(mult = c(0.02, 0.02), add = 0.1)
      )
    )
  ) +
  theme(
    text = element_text(family = "Arial"),
    strip.text.x = element_text(size = 12, face = "bold", margin = margin(t = 5, b = 5)),
    strip.text.y = element_text(size = 12, face = "bold", margin = margin(r = 5, l = 5))
  )


cairo_pdf('../../data/05.Summarize_biological_metrics/05.BIO_ALL_COMBINED_per_track_horizontal.pdf',
          width = my_plotting_params[['single_plot_width']]*1, height = my_plotting_params[['single_plot_height']]/1.5, family = my_plotting_params[['font']])
print(p)
dev.off()



### 02. only plot speed
# Base plot
p <- ggplot(df_plot[df_plot$metric == "speed", ], aes(y = y_rank)) +
  # thin 95% bar
  geom_errorbarh(
    aes(xmin = q0025/1000, xmax = q0975/1000, colour = "Simulated 95% (2.5–97.5%)"),
    height = 0
  ) +
  # thick 50% bar
  geom_errorbarh(
    aes(xmin = q0250/1000, xmax = q0750/1000, colour = "Simulated 50% (25–75%)"),
    height = 0, linewidth = 1.2
  ) +
  # red dot
  geom_point(
    aes(x = real/1000, colour = "Empirical value for each track"),
    size = 1.6, alpha = 0.7
  ) +
  labs(
    x = "Migration speed (km/day)", y = NULL,
    colour = NULL   # legend title
  ) +
  scale_colour_manual(
    values = c(
      "Simulated 95% (2.5–97.5%)" = "grey55",
      "Simulated 50% (25–75%)"    = "grey35",
      "Empirical value for each track"          = "red"
    )
  ) +
  guides(
    colour = guide_legend(ncol = 1)  # force vertical stacking
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.placement    = "outside",
    strip.background   = element_blank(),
    # legend.position    = "bottom"   # or "right"
    legend.position = c(0.87, 0.2),
    legend.text = element_text(size = 12, margin = margin(t = 5, b = 5)),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, b = 5))
  ) +
  facet_wrap(~ sp, nrow = 2, scales = "free") +
  scale_y_continuous(breaks = NULL, expand = expansion(add = 0.5))  +
  theme(
    text = element_text(family = "Arial"),
    strip.text.x = element_text(size = 12, margin = margin(t = 5, b = 5)),
    strip.text.y = element_text(size = 12, margin = margin(r = 5, l = 5))
  ) +
  theme(
    panel.spacing.x = unit(0.4, "cm"),
    panel.spacing.y = unit(0.2, "cm"),
    axis.text.x = element_text(size = 10, margin = margin(t = 1))
  ) +
  coord_cartesian(clip = "off")  # prevents first speed label from being clipped


cairo_pdf('../../data/05.Summarize_biological_metrics/05.BIO_ALL_COMBINED_per_track_horizontal_speed_only.pdf',
          width = my_plotting_params[['single_plot_width']]*1, height = my_plotting_params[['single_plot_height']]/1.7, family = my_plotting_params[['font']])
print(p)
dev.off()


####### 03. Only plot straightness and n_stopovers
# Base plot
df_plot <- df_plot |>
  # filter(metric %in% c("straightness", "n_stopovers")) |>
  mutate(
    collapsed95 = ifelse(q0025 == q0975, 1, 0),
    collapsed50 = ifelse(q0250 == q0750, 1, 0)
  )

p <- ggplot(df_plot[df_plot$metric %in% c('straightness', 'n_stopovers'), ], aes(y = y_rank)) +
  geom_errorbarh(aes(xmin = q0025, xmax = q0975, colour = "Simulated 95% (2.5–97.5%)"),
                 height = 0) +
  geom_errorbarh(aes(xmin = q0250, xmax = q0750, colour = "Simulated 50% (25–75%)"),
                 height = 0, linewidth = 1.2) +
  geom_point(
    aes(x = q0025, alpha=collapsed95),
    colour = "grey35",
    size = 2
  ) +
  scale_alpha_continuous(range = c(0, 1), guide = "none") +
  geom_point(aes(x = real, colour = "Empirical value for each track"), size = 1.6, alpha = 0.7) +
  labs(
    x = NULL, y = NULL,
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.placement    = "outside",
    strip.background   = element_blank()
  ) +
  ggh4x::facet_grid2(
    rows  = vars(metric),
    cols  = vars(sp),
    scales = "free",          # free both axes
    independent = "all",      # ← FIX: make x and y independent per panel
    labeller = labeller(metric = metric_labels)
  ) +
  scale_y_continuous(breaks = NULL, expand = expansion(add = 0.5))  +
  scale_colour_manual(
    values = c(
      "Simulated 95% (2.5–97.5%)" = "grey55",
      "Simulated 50% (25–75%)"    = "grey35",
      "Empirical value for each track"          = "red"
    )
  )


# 2) make columns a bit farther apart + allow labels to render outside panels
p <- p +
  theme(
    panel.spacing.x = unit(0.4, "cm"),
    panel.spacing.y = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8, margin = margin(t = 1)),
    legend.position    = "bottom",
    legend.title = element_text(),
    legend.text = element_text(size = 12, margin = margin(t = 5, b = 5))
  ) +
  labs(colour = "") +
  coord_cartesian(clip = "off")  # prevents first speed label from being clipped

# 3) per-row x scales: keep straightness/stopovers as-is, adjust speed
p <- p +
  ggh4x::facetted_pos_scales(
    x = list(
      metric == "straightness" ~ scale_x_continuous(
        breaks = breaks_inset_3,
        labels = scales::label_number(accuracy = 0.1),
        expand = expansion(mult = c(0.02, 0.02), add = 0.1)
      ),
      metric == "n_stopovers" ~ scale_x_continuous(
        breaks = scales::breaks_pretty(n = 3),
        labels = scales::label_number(accuracy = 0.1),
        expand = expansion(mult = c(0.02, 0.02), add = 0.1)
      ),
      # SPEED: inset breaks (no border ticks) + extra expansion + scientific labels
      metric == "speed" ~ scale_x_continuous(
        breaks = breaks_inset_3,
        labels = scales::label_scientific(digits = 1),
        expand = expansion(mult = c(0.02, 0.02), add = 0.1)
      )
    )
  ) +
  theme(
    text = element_text(family = "Arial"),
    strip.text.x = element_text(size = 12, face = "bold", margin = margin(t = 5, b = 5)),
    strip.text.y = element_text(size = 12, face = "bold", margin = margin(r = 5, l = 5))
  )

cairo_pdf('../../data/05.Summarize_biological_metrics/05.BIO_ALL_COMBINED_per_track_horizontal_straightness_and_n_stopovers.pdf',
          width = my_plotting_params[['single_plot_width']]*1, height = my_plotting_params[['single_plot_height']]/1.5, family = my_plotting_params[['font']])
print(p)
dev.off()

### Summarize quantitatively
calibration_summary <- df_plot |>
  dplyr::group_by(sp, metric) |>
  dplyr::summarize('95cali'=mean(real>=q0025 & real<=q0975),
                   '50cali'=mean(real>=q0250 & real<=q0750)) |>
  dplyr::ungroup() |>
  dplyr::arrange(metric, sp)

write.csv(calibration_summary, '../../data/05.Summarize_biological_metrics/biometrics_calibration_summary.csv')


## Write MSE files
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



### 04. Make other plots (not used in the final manuscripts)
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
pdf(glue::glue("../../data/05.Summarize_biological_metrics/01.bio_sanity_check.pdf"), width = my_plotting_params[['single_plot_width']]*1*n_col, height = my_plotting_params[['single_plot_height']]*1.2*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = all_plots, ncol = n_col)  # Adjust 'ncol' to control the number of plots per row
dev.off()


### 05. Make it in one plot: migration speed
new_all_conditional_rts_stats_res <- 
  all_conditional_rts_stats_res[,c('sp', 'common_name', 'speed_synth', 'speed_real')] |>
  na.omit()
cor_stats <- new_all_conditional_rts_stats_res |>
  group_by(common_name) |>
  summarize(
    r = cor(speed_real, speed_synth),
    .groups = "drop"
  )
overall_cor_stats <- new_all_conditional_rts_stats_res |>
  summarize(
    r = cor(speed_real, speed_synth)
  )
overall_r <- overall_cor_stats$r
mean_r  <- mean(cor_stats$r)
range_r <- range(cor_stats$r)
annot_text <- sprintf(
  "Overall Pearson's r = %.2f\nMean Pearson's r = %.2f\nRange = %.2f–%.2f",
  overall_r, mean_r, range_r[1], range_r[2]
)
library(grid)
lbl <- textGrob(
  annot_text,
  x = unit(1, "npc") - unit(17.5, "lines"),
  y = unit(18, "lines"),
  just = c("left", "top"),
  gp = gpar(fontsize = 16)
)


trans_breaks_x <- seq(
  sqrt(min(new_all_conditional_rts_stats_res$speed_real) + 1),
  sqrt(max(new_all_conditional_rts_stats_res$speed_real) + 1),
  length.out = 5
)
orig_breaks_x <- trans_breaks_x**2 - 1

trans_breaks_y <- seq(
  sqrt(min(new_all_conditional_rts_stats_res$speed_synth) + 1),
  sqrt(max(new_all_conditional_rts_stats_res$speed_synth) + 1),
  length.out = 5
)
orig_breaks_y <- trans_breaks_y**2 - 1

p_speed <- ggplot(new_all_conditional_rts_stats_res, 
       aes(x = (speed_real+1)^(1/2), y=(speed_synth+1)^(1/2), color = common_name, fill = common_name)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method='lm', aes(color=common_name), alpha = 0.1) +
  scale_color_brewer(palette = "Paired", name = "Species") +
  scale_fill_brewer(palette = "Paired", name = "Species") +
  labs(x = 'Observed migration speed (km/day)', y = 'Modeled migration speed\n(km/day)') +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red2",
              linetype = "dashed") +
  scale_x_continuous(
    breaks = trans_breaks_x,
    labels = round(orig_breaks_x/1000, 0)
  ) +
  scale_y_continuous(
    breaks = trans_breaks_y,
    labels = round(orig_breaks_y/1000, 0)
  ) +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']] +
  annotation_custom(lbl)
p_speed


# save
cairo_pdf('../../data/05.Summarize_biological_metrics/02.migration_speed_all_in_one.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p_speed)
dev.off()


### 06. Make it in one plot: n_stopovers
new_all_conditional_rts_stats_res <- 
  all_conditional_rts_stats_res[,c('sp', 'common_name', 'n_stopovers_synth', 'n_stopovers_real')] |>
  na.omit()
cor_stats <- new_all_conditional_rts_stats_res |>
  group_by(common_name) |>
  summarize(
    r = cor(n_stopovers_real, n_stopovers_synth),
    .groups = "drop"
  )
cor_stats <- na.omit(cor_stats)
overall_cor_stats <- new_all_conditional_rts_stats_res |>
  summarize(
    r = cor(n_stopovers_real, n_stopovers_synth)
  )
overall_r <- overall_cor_stats$r
mean_r  <- mean(cor_stats$r)
range_r <- range(cor_stats$r)
annot_text <- sprintf(
  "Overall Pearson's r = %.2f\nMean Pearson's r = %.2f\nRange = %.2f–%.2f",
  overall_r, mean_r, range_r[1], range_r[2]
)
library(grid)
lbl <- textGrob(
  annot_text,
  x = unit(1, "npc") - unit(17.5, "lines"),
  y = unit(18, "lines"),
  just = c("left", "top"),
  gp = gpar(fontsize = 16)
)

p_stopover <- ggplot(new_all_conditional_rts_stats_res, 
            aes(x = n_stopovers_real, y=n_stopovers_synth, color = common_name, fill = common_name)) +
  geom_jitter(alpha = 0.5, width=0.1, height = 0.1) +
  geom_smooth(method='lm', aes(color=common_name), alpha = 0.1) +
  scale_color_brewer(palette = "Paired", name = "Species") +
  scale_fill_brewer(palette = "Paired", name = "Species") +
  labs(x = 'Observed number of stopovers\n(staylength > 7 days)', y = 'Modeled number of stopovers\n(staylength > 7 days)') +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red2",
              linetype = "dashed") +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']] +
  annotation_custom(lbl)
p_stopover

# save
cairo_pdf('../../data/05.Summarize_biological_metrics/03.n_stopovers_all_in_one.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p_stopover)
dev.off()


### 07. Make it in one plot: straightness
new_all_conditional_rts_stats_res <- 
  all_conditional_rts_stats_res[,c('sp', 'common_name', 'straightness_synth', 'straightness_real')] |>
  na.omit()
cor_stats <- new_all_conditional_rts_stats_res |>
  group_by(common_name) |>
  summarize(
    r = cor(straightness_real, straightness_synth),
    .groups = "drop"
  )
overall_cor_stats <- new_all_conditional_rts_stats_res |>
  summarize(
    r = cor(straightness_real, straightness_synth)
  )
overall_r <- overall_cor_stats$r
mean_r  <- mean(cor_stats$r)
range_r <- range(cor_stats$r)
annot_text <- sprintf(
  "Overall Pearson's r = %.2f\nMean Pearson's r = %.2f\nRange = %.2f–%.2f",
  overall_r, mean_r, range_r[1], range_r[2]
)
library(grid)
lbl <- textGrob(
  annot_text,
  x = unit(1, "npc") - unit(17.5, "lines"),
  y = unit(18, "lines"),
  just = c("left", "top"),
  gp = gpar(fontsize = 16)
)


p_straightness <- ggplot(new_all_conditional_rts_stats_res, 
            aes(x = straightness_real, y=straightness_synth, color = common_name, fill = common_name)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method='lm', aes(color=common_name), alpha = 0.1) +
  scale_color_brewer(palette = "Paired", name = "Species") +
  scale_fill_brewer(palette = "Paired", name = "Species") +
  labs(x = 'Observed route straightness', y = 'Modeled route straightness') +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red2",
              linetype = "dashed") +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']] +
  annotation_custom(lbl)
p_straightness

# save
cairo_pdf('../../data/05.Summarize_biological_metrics/04.straightness_all_in_one.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()

### 08. Combine together
library(patchwork)
pp <- p_speed + theme(legend.position = "none") + theme(plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
  p_stopover + theme(legend.position = "none") + theme(plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) + 
  p_straightness + theme(plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) + 
  plot_annotation(tag_levels = list(c('(a)','(b)', '(c)'))) & 
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.tag.position  = c(0.01, 1)
  )

# save
cairo_pdf('../../data/05.Summarize_biological_metrics/04.BIO_ALL_COMBINED.pdf',
          width = my_plotting_params[['single_plot_width']]*2.5, height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(pp)
dev.off()

