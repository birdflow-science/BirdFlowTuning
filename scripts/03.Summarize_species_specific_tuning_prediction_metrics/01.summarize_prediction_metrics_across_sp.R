library(ggplot2)
setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/03.Summarize_species_specific_tuning_prediction_metrics/')
source('../plotting_params/plotting_params.R')
results <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
results <- results |> dplyr::group_by(.data[['sp']], .data[['method']]) |> dplyr::slice(1) |> dplyr::ungroup()
# write.csv(as.data.frame(results[results$method=='ST098_and_LL',]),
#           '/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/best_model_set/best_models_byST098_and_LL.csv')

## Plot 1: Species specific-tuning actually works!
p <- ggplot(data=results[results$method=='ST098_and_LL',], aes(x=.data[['weighted_mean_ll_improvement']])) +
  geom_histogram() +
  my_plotting_params[['zero_vline']] +
  labs(x = "Weighted average\nlog likelihood Improvement", y = 'Species count') +
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
  ggrepel::geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 25) +
  labs(x = "Weighted average\nlog likelihood Improvement", y = 'Distance gain (km)') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]
  
# save
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/01.03.scatter_LL_and_distance_metric.pdf',
          width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
print(p)
dev.off()

## Plot 4: Put them in one plot
p <- ggplot(data=results[results$method=='ST098_and_LL',], 
            aes(x=.data[['weighted_mean_ll_improvement']], y=.data[['weighted_mean_win_distance']])) +
  my_plotting_params[['scatter']]+
  my_plotting_params[['zero_vline']] +
  my_plotting_params[['zero_hline']] +
  ylim(-500,NA)+
  # ggrepel::geom_text_repel(data=results[(results$method=='ST098_and_LL') & (results$weighted_mean_win_distance>3000),],
  #                          aes(label = common_name), size = 3, max.overlaps = 20, max.time = 3) +
  # ggrepel::geom_text_repel(data=results[(results$method=='ST098_and_LL') & (results$weighted_mean_win_distance<=3000),],
  #                          aes(label = common_name), size = 3, max.overlaps = 30, box.padding = 0.4, max.time = 3) +
  ggrepel::geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 30, max.time = 3, force=5, force_pull=5,
                           max.iter=5000) +
  ggrepel::geom_text_repel(data=results[(results$method=='ST098_and_LL') & (results$weighted_mean_win_distance<1000) & (results$weighted_mean_ll_improvement>3) & (results$weighted_mean_ll_improvement<6),],
                           aes(label = common_name), size = 3, max.overlaps = 20, box.padding = 0.25, max.time = 3, force=5, force_pull=5,
                           max.iter=5000) +
  labs(x = "Weighted average\nlog likelihood Improvement", y = 'Distance gain (km)') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]
  
p <- ggExtra::ggMarginal(p, type = "histogram", fill='gray50')

# save
cairo_pdf('../../data/04.Sumamrize_prediction_metrics/01.04.scatter_and_hist_in_same_plot.pdf',
          width = my_plotting_params[['single_plot_width']]*0.7, height = my_plotting_params[['single_plot_height']]*0.7, family = my_plotting_params[['font']])
print(p)
dev.off()


