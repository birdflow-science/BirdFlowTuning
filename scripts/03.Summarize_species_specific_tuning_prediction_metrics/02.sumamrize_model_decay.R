library(ggplot2)
library(ggeffects)
library(lme4)
library(ggeffects)
library(minpack.lm)
library(dplyr)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/03.Summarize_species_specific_tuning_prediction_metrics/')
source('../plotting_params/plotting_params.R')
source('../02.Summarize_validation_preliminary/load_data_functions.R')
results <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
results <- results |> dplyr::group_by(.data[['sp']], .data[['method']]) |> dplyr::slice(1) |> dplyr::ungroup()
results <- results[results$training_n_intervals>10,]


fit_exponential_model <- function(tmp, x='elapsed_days', y='win_distance',
                                  B_initial = 0.001) {
  tmp$x <- tmp[[x]]
  tmp$y <- tmp[[y]]
  # w <- tmp[[weight]] + 1e-16
  # w <- w / sum(w)

  start_vals <- list(
    A = max(tmp$y) - min(tmp$y),
    B = B_initial,   # rough guess for decay rate
    C = 0        # offset guess
  )
  fit <- nlsLM(
    y ~ A * exp(-B * x) + C,
    data = tmp,
    start = start_vals,
    lower  = c(A = 0, B = 0, C = -Inf),
    upper  = c(A = Inf, B = Inf, C = 0),
    control=nls.lm.control(maxiter=1024),
    # weights =  w
  )
  return(fit)
}


get_x_intercept <- function(fit, threshold=150) {
  coefs <- coef(fit)
  A <- coefs["A"]
  B <- coefs["B"]
  C <- coefs["C"]

  # check that a solution exists: threshold must lie between C and A+C
  if (threshold <= C) {
    warning("Threshold is below the horizontal asymptote C; the curve never goes below this value.")
    return(NA_real_)
  } else if (threshold >= A + C) {
    # stop("Threshold is above the starting value A+C; the curve starts below this value.")
    x_thresh <- 0
    return(x_thresh)
  }
  
  # solve for x
  x_thresh <- -log((threshold - C) / A) / B
  return(x_thresh)
}

regress_exponential_decay <- function(data,
                                  response_var="win",
                                  predictor_var="elapsed",
                                  random_effect_var = "common_name",
                                  x_name = 'Time horizon (days)',
                                  y_name = "Win in distance (km)",
                                  threshold = 150,
                                  xmax=4000
){
  
  # each regression line for each species
  regression_result <- data |>
    group_by(.data[[random_effect_var]]) |>
    group_modify(~ {
      # Fit the model within each group
      safe_fit <- purrr::safely(fit_exponential_model)
      out <- safe_fit(.x, x=predictor_var, y=response_var)
      if (is.null(out$result)) {
        return(data.frame(list(A=NA, B=NA, C=NA, x_intercept=NA)))
      } else {
        co <- coef(out$result)
        x_intercept <- get_x_intercept(out$result, threshold=threshold)
        return(data.frame(list(A=co["A"], B=co["B"], C=co["C"], x_intercept=x_intercept)))
      }
    })
  
  regression_result_na_removed <- regression_result |> dplyr::filter(!is.na(A), !is.na(B), !is.na(C))
  
  new_x <- seq(min(data[[predictor_var]]), min(max(data[[predictor_var]]), xmax), length.out=100)
  
  preds_by_sp <- regression_result_na_removed |>
    group_by(.data[[random_effect_var]]) |>
    group_modify(~ {
      A <- .x$A
      B <- .x$B
      C <- .x$C
      data.frame(
        x    = new_x,
        pred = A * exp(-B * new_x) + C
      )
    })
  
  combined_pred <- as.data.frame(list(
    x = new_x,
    pred = median(regression_result_na_removed$A) *
      exp(-median(regression_result_na_removed$B) * new_x) +
      median(regression_result_na_removed$C)
  ))
  
  # Plot the regression line with confidence intervals
  p <- ggplot() +
    geom_line(data=preds_by_sp, aes(x=x, y = pred, group = .data[[random_effect_var]]), alpha=0.1) +
    geom_line(data=combined_pred, aes(x = x, y = pred), color = "steelblue", linewidth = 2) +
    labs(
      x = x_name,
      y = y_name
    ) +
    my_plotting_params[['zero_hline']] +
    my_plotting_params[['theme']] +
    my_plotting_params[['formater']] +
    xlim(0, xmax)
  # 
  
  return(list(p=p, regression_result=regression_result))
}


## Load all transition data
all_transitions <- list()
for (sp in results$sp |> unique()) {
  model_name <- c(results[(results$sp==sp) & (results$method=='ST098_and_LL'),'model'])[1]
  transition_file <- glue::glue('{parent_path}/{sp}/{sp}_150km_interval_based_eval_using_migration_transitions/each_transition_evaluation/test_each_transition_evaluation_all_combined_{model_name}.rds')
  transitions <- readRDS(transition_file)
  if (nrow(transitions) < 10) {
    next
  }
  if (sum(transitions$elapsed_km>0)<5) {
    next
  }
  if ('effective_win_distance' %in% colnames(transitions)) {
    transitions <- transitions[,!(names(transitions) %in% "effective_win_distance")]
  }
  transitions$sp <- sp
  all_transitions[[length(all_transitions) + 1]] <- transitions
}

all_transitions <- do.call(rbind, all_transitions)
all_transitions$LL_improvement <- all_transitions$ll - all_transitions$null_ll
all_transitions <- all_transitions[all_transitions$elapsed_km < 5000,]

## plot 1
res <- regress_exponential_decay(
  data=all_transitions,
  predictor_var="elapsed_days",
  response_var="win_distance",
  random_effect_var = "sp",
  x_name = 'Time horizon (days)',
  y_name = "Distance gain (km)",
  threshold=150,
  xmax=max(all_transitions$elapsed_days)
)
p <- res[['p']]
regression_result <- res[['regression_result']]

# save
write.csv(regression_result, '../../data/04.Sumamrize_prediction_metrics/02.01.regression_distance_gain_elapsed_days.csv')
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/02.01.regression_distance_gain_elapsed_days.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()


## plot 2
res <- regress_exponential_decay(
  data=all_transitions,
  predictor_var="elapsed_km",
  response_var="win_distance",
  random_effect_var = "sp",
  x_name = 'Distance horizon (km)',
  y_name = "Distance gain (km)",
  threshold=150,
  xmax=max(all_transitions$elapsed_km)
)
p <- res[['p']]
regression_result <- res[['regression_result']]

# save
write.csv(regression_result, '../../data/04.Sumamrize_prediction_metrics/02.02.regression_distance_gain_elapsed_km.csv')
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/02.02.regression_distance_gain_elapsed_km.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()


## plot 3
res <- regress_exponential_decay(
  data=all_transitions,
  predictor_var="elapsed_days",
  response_var="win_distance_fraction",
  random_effect_var = "sp",
  x_name = 'Time horizon (days)',
  y_name = "Relative distance gain",
  threshold=0.05,
  xmax=max(all_transitions$elapsed_days)
)
p21 <- res[['p']]
regression_result <- res[['regression_result']]

# save
write.csv(regression_result, '../../data/04.Sumamrize_prediction_metrics/02.03.regression_relative_distance_gain_elapsed_days.csv')
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/02.03.regression_relative_distance_gain_elapsed_days.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p21)
dev.off()


## plot 4
res <- regress_exponential_decay(
  data=all_transitions,
  predictor_var="elapsed_km",
  response_var="win_distance_fraction",
  random_effect_var = "sp",
  x_name = 'Distance horizon (km)',
  y_name = "Relative distance gain",
  threshold=0.05,
  xmax=max(all_transitions$elapsed_km)
)
p22 <- res[['p']]
regression_result <- res[['regression_result']]

# save
write.csv(regression_result, '../../data/04.Sumamrize_prediction_metrics/02.04.regression_relative_distance_gain_elapsed_km.csv')
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/02.04.regression_relative_distance_gain_elapsed_km.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p22)
dev.off()


## plot 5
res <- regress_exponential_decay(
  data=all_transitions,
  predictor_var="elapsed_days",
  response_var="LL_improvement",
  random_effect_var = "sp",
  x_name = 'Time horizon (days)',
  y_name = "Log likelihood improvement",
  threshold=0.05,
  xmax=max(all_transitions$elapsed_days)
)
p11 <- res[['p']]
regression_result <- res[['regression_result']]

# save
write.csv(regression_result, '../../data/04.Sumamrize_prediction_metrics/02.05.regression_LL_elapsed_days.csv')
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/02.05.regression_LL_elapsed_days.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p11)
dev.off()


## plot 6
res <- regress_exponential_decay(
  data=all_transitions,
  predictor_var="elapsed_km",
  response_var="LL_improvement",
  random_effect_var = "sp",
  x_name = 'Distance horizon (km)',
  y_name = "Log likelihood improvement",
  threshold=0.05,
  xmax=800
)
p12 <- res[['p']]
regression_result <- res[['regression_result']]

# save
write.csv(regression_result, '../../data/04.Sumamrize_prediction_metrics/02.06.regression_LL_elapsed_km.csv')
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/02.06.regression_LL_elapsed_km.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p12)
dev.off()

## plot 7: summarize model decay x_intercept
decay_intercept <- read.csv('../../data/04.Sumamrize_prediction_metrics/02.04.regression_relative_distance_gain_elapsed_km.csv')
decay_intercept <- na.omit(decay_intercept)
decay_intercept <- decay_intercept[decay_intercept$x_intercept<=10000,]
mean_dist <- decay_intercept$x_intercept |> na.omit() |> mean()
p23 <- ggplot(data=decay_intercept, aes(x=.data[['x_intercept']])) +
  geom_histogram() +
  my_plotting_params[['zero_vline']] +
  labs(x = "Maximum functional distance horizon (km)\n for relative distance gain", y = 'Species count') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]  +
  annotate(
    "label",
    x     = Inf, 
    y     = Inf, 
    label = paste0("Mean: ", sprintf("%.2f", mean_dist), ' km'),
    hjust = 1.1, 
    vjust = 1.5, 
    fill= NA,
    label.size = 0,
    size=8
  )

# save
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/02.07.hist_regression_relative_distance_gain_elapsed_km.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p23)
dev.off()

## plot 8
decay_intercept <- read.csv('../../data/04.Sumamrize_prediction_metrics/02.02.regression_distance_gain_elapsed_km.csv')
decay_intercept <- na.omit(decay_intercept)
decay_intercept <- decay_intercept[decay_intercept$x_intercept<=10000,]
mean_dist <- decay_intercept$x_intercept |> na.omit() |> mean()
p <- ggplot(data=decay_intercept, aes(x=.data[['x_intercept']])) +
  geom_histogram() +
  my_plotting_params[['zero_vline']] +
  labs(x = "Maximum functional distance horizon (km)\n for distance gain", y = 'Species count') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]+
  xlim(NA, 6646.544 +1)  +
  annotate(
    "label",
    x     = Inf, 
    y     = Inf, 
    label = paste0("Mean: ", sprintf("%.2f", mean_dist), ' km'),
    hjust = 1.1, 
    vjust = 1.5, 
    fill= NA,
    label.size = 0,
    size=8
  )
p

# save
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/02.08.hist_regression_distance_gain_elapsed_km.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()

### plot 9
decay_intercept <- read.csv('../../data/04.Sumamrize_prediction_metrics/02.06.regression_LL_elapsed_km.csv')
decay_intercept <- na.omit(decay_intercept)
decay_intercept <- decay_intercept[decay_intercept$x_intercept<=10000,]
mean_dist <- decay_intercept$x_intercept |> na.omit() |> mean()
p13 <- ggplot(data=decay_intercept, aes(x=.data[['x_intercept']])) +
  geom_histogram() +
  my_plotting_params[['zero_vline']] +
  labs(x = "Maximum functional distance horizon (km)\n for log likelihood improvement", y = 'Species count') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']] +
  xlim(NA, 6646.544 +1) +
  annotate(
    "label",
    x     = Inf, 
    y     = Inf, 
    label = paste0("Mean: ", sprintf("%.2f", mean_dist), ' km'),
    hjust = 1.1, 
    vjust = 1.5, 
    fill= NA,
    label.size = 0,
    size=8
  )

# save
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/02.08.hist_regression_LL_elapsed_km.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p13)
dev.off()



#### Combine together
library(patchwork)
pp <- (p11 + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + p12 + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + p13 + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")))/
  (p21 + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + p22 + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + p23 + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")))

pp <- pp + plot_annotation(tag_levels = list(c('(a)','(b)','(c)','(d)','(e)','(f)'))) & 
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.tag.position  = c(0.01, 1)
  )

cairo_pdf('../../data/04.Sumamrize_prediction_metrics/ALL_model_decay_COMBINED.pdf',
          width = my_plotting_params[['single_plot_width']]*2.5, height = my_plotting_params[['single_plot_height']]*1.3, family = my_plotting_params[['font']])
print(pp)
dev.off()





