library(ggplot2)
setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/03.Summarize_species_specific_tuning_prediction_metrics/')
source('../plotting_params/plotting_params.R')
results <- read.csv('../../data/03.All_validation_summary/validation_final_summary_filtered.csv')
results <- results |> dplyr::group_by(.data[['sp']]) |> dplyr::slice(1) |> dplyr::ungroup()

## Plot 1: Species specific-tuning actually works!
p <- ggplot(data=results[results$method=='ST098_and_LL',], aes(x=.data[['weighted_mean_ll_improvement']])) +
  geom_histogram() +
  my_plotting_params[['zero_vline']] +
  labs(x = "Weight average log likelihood Improvement", y = 'Species count') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

# save
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/01.01.hist_LL.pdf',
    width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()

## Plot 2: Species specific-tuning actually works! (but using times X)
p <- ggplot(data=results[results$method=='ST098_and_LL',], aes(x=.data[['weighted_mean_ll_improvement']])) +
  geom_histogram() +
  my_plotting_params[['zero_vline']] +
  labs(x = "How many times BirdFlow model predictions\nare better than baseline", y = 'Species count') +
  scale_x_continuous(
    labels = function(x) round(exp(x), 1)
  ) +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

# save
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/01.02.hist_LL_intuitive_label.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()

## Plot 3: Species specific-tuning actually works! (consider both LL and distance_metric)
p <- ggplot(data=results[results$method=='ST098_and_LL',], 
            aes(x=.data[['weighted_mean_ll_improvement']], y=.data[['weighted_mean_win_distance']])) +
  my_plotting_params[['scatter']]+
  my_plotting_params[['zero_vline']] +
  my_plotting_params[['zero_hline']] +
  labs(x = "Weight average log likelihood Improvement", y = 'Distance gain (km)') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

# save
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/01.03.scatter_LL_and_distance_metric.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()

