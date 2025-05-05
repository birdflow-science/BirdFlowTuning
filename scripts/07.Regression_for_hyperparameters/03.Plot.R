library(ggplot2)
RF_metrics_summary <- read.csv('../../data/08.Regression_for_hyperparameters/all_metrics.csv')

source('../plotting_params/plotting_params.R')

## 01. plot randomforest fits
param_count <- 0
for (param_name in c('ent', 'pow', 'dist')) {
  modeled_results_train <- read.csv(glue::glue('../../data/08.Regression_for_hyperparameters/train_y_true_y_pred_df_{param_name}.csv'))
  modeled_results_test <- read.csv(glue::glue('../../data/08.Regression_for_hyperparameters/test_y_true_y_pred_df_{param_name}.csv'))
  param_count <- param_count+1
  
  p <- ggplot() + 
    geom_jitter(data=modeled_results_train, aes(x=y_train, y=y_train_pred),
                shape = 21, size=3, alpha=0.9,
                fill = "steelblue",
                color = "black",
                stroke = 0.5,
                width = 0, height = 0) +
    geom_smooth(data=modeled_results_train, aes(x=y_train, y=y_train_pred, color='Train'), method='lm', fill='steelblue', alpha=0.2) + 
    geom_jitter(data=modeled_results_test, aes(x=y_test, y=y_test_pred),
                  shape = 21, size=3, alpha=0.9,
                  fill = "orange2",
                  color = "black",
                  stroke = 0.5,
                  width = 0, height = 0) +
    geom_smooth(data=modeled_results_test, aes(x=y_test, y=y_test_pred, color='Test'), method='lm', fill='orange2', alpha=0.2) +
    scale_color_manual(
      name   = "Phase",
      values = c(
        "Train" = "steelblue",
        "Test"  = "orange2"
      ),
      breaks = c("Train","Test")
    ) +
    labs(x='Observed hyperparameters', y='Modeled hyperparameters') +
    annotate(
      "label",
      x     = -Inf, 
      y     = Inf, 
      label = param_name,
      hjust = -0.5, 
      vjust = 1.5, 
      fill= NA,
      label.size = 0,
      size=10
    ) +
    annotate(
      "label",
      x     = -Inf, 
      y     = Inf,
      label = paste0('Test set R\u00B2: ',
                     round(RF_metrics_summary[RF_metrics_summary$hyperparameter==param_name,]$r2_test, 2)),
      hjust = -2, 
      vjust = 18, 
      fill= NA,
      label.size = 0,
      size=5
    ) +
    my_plotting_params[['theme']] +
    my_plotting_params[['formater']]

  cairo_pdf(glue::glue('../../data/08.Regression_for_hyperparameters/0{param_count}.RF_fit_{param_name}.pdf'),
            width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
  print(p)
  dev.off()
}

## 02. plot correlation: morphology and hyperparameters
validation_summary <- read.csv('../../data/08.Regression_for_hyperparameters/Best_models_by_ST098_and_LL_validation_summaries_with_range_stats.csv')
validation_summary <- validation_summary |> dplyr::group_by(.data[['sp']]) |> dplyr::slice(1)
validation_summary['ent'] = validation_summary['ent']/(validation_summary['ent'] + validation_summary['dist'])
validation_summary['dist'] = validation_summary['dist']/(validation_summary['ent'] + validation_summary['dist'])

## p1
p <- ggplot(data=validation_summary, aes(x=Mass, y=ent, fill=ORDER1_eBird))+
  geom_jitter(shape = 21, size=3, alpha=0.9,
              color = "black",
              stroke = 0.5,
              width = 0, height = 0.01) +
  scale_fill_brewer(palette = "Paired", name = "Order") +
  geom_smooth(method='lm', fill='steelblue', color='steelblue', alpha=0.2) +
  scale_x_log10() +
  labs(x='Body mass (g)') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

cairo_pdf('../../data/08.Regression_for_hyperparameters/04.Mass_ent_regression.pdf',
          width = my_plotting_params[['single_plot_width']]*1.5, height = my_plotting_params[['single_plot_height']]*1.5, family = my_plotting_params[['font']])
print(p)
dev.off()

## p2
p <- ggplot(data=validation_summary, aes(x=Mass, y=dist, fill=ORDER1_eBird))+
  geom_jitter(shape = 21, size=3, alpha=0.9,
              color = "black",
              stroke = 0.5,
              width = 0, height = 0.01) +
  scale_fill_brewer(palette = "Paired", name = "Order") +
  geom_smooth(method='lm', fill='steelblue', color='steelblue', alpha=0.2) +
  scale_x_log10() +
  labs(x='Body mass (g)') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

cairo_pdf('../../data/08.Regression_for_hyperparameters/05.Mass_dist_regression.pdf',
          width = my_plotting_params[['single_plot_width']]*1.5, height = my_plotting_params[['single_plot_height']]*1.5, family = my_plotting_params[['font']])
print(p)
dev.off()

## p3
p <- ggplot(data=validation_summary, aes(x=postbreeding_abundance_variation, y=pow, fill=ORDER1_eBird))+
  geom_jitter(shape = 21, size=3, alpha=0.9,
              color = "black",
              stroke = 0.5,
              width = 0, height = 0.01) +
  scale_fill_brewer(palette = "Paired", name = "Order") +
  geom_smooth(method='lm', fill='steelblue', color='steelblue', alpha=0.2) +
  scale_x_log10() +
  labs(x='Spatial unevenness of average\nabundance during post-breeding migration') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']]

cairo_pdf('../../data/08.Regression_for_hyperparameters/06.postbreeding_abundance_variation_pow_regression.pdf',
          width = my_plotting_params[['single_plot_width']]*1.5, height = my_plotting_params[['single_plot_height']]*1.5, family = my_plotting_params[['font']])
print(p)
dev.off()


## 03. PCA plot
validation_summary <- read.csv('../../data/08.Regression_for_hyperparameters/Best_models_by_ST098_and_LL_validation_summaries_with_range_stats.csv')
validation_summary <- validation_summary |> dplyr::group_by(.data[['sp']]) |> dplyr::slice(1)
validation_summary['ent'] = validation_summary['ent']/(validation_summary['ent'] + validation_summary['dist'])
validation_summary['dist'] = validation_summary['dist']/(validation_summary['ent'] + validation_summary['dist'])

pca_res <- prcomp(validation_summary[,c('ent','dist', 'pow')], 
                  center = TRUE,
                  scale. = TRUE)
pca_res
ss <- summary(pca_res)
print(ss)

new_validation_summary <- cbind(validation_summary, as.data.frame(pca_res$x))



p <- ggplot(data=new_validation_summary, aes(x=PC1, y=PC2, fill=ORDER1_eBird))+
  geom_jitter(shape = 21, size=3, alpha=0.9,
              color = "black",
              stroke = 0.5,
              width = 0.1, height = 0.1) +
  scale_fill_brewer(palette = "Paired", name = "Order") +
  ggrepel::geom_text_repel(aes(label = common_name), size = 3, max.overlaps = 6) +
  ggrepel::geom_text_repel(data=new_validation_summary[new_validation_summary$PC2 < -1.2,],
                           aes(label = common_name), size = 3, max.overlaps = 23) +
  ggrepel::geom_text_repel(data=new_validation_summary[new_validation_summary$PC1 < -2,],
                           aes(label = common_name), size = 3, max.overlaps = 23) +
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
    axis.title.y = element_text(size = 18, margin = margin(r = 20)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  labs(
    x=paste0('PC1 (', round(ss$importance['Proportion of Variance','PC1']*100, 2), '%)'),
    y=paste0('PC2 (', round(ss$importance['Proportion of Variance','PC2']*100, 2), '%)')
  ) +
  my_plotting_params[['formater']] +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(t = 5, r = 5, b = 80, l = 40, unit = "pt"),
    axis.title.x    = element_text(margin = margin(t = 25))
  ) +
  annotate("segment",
           x    = -2.5, xend =  5,
           y    = -Inf, yend = -Inf,
           arrow = arrow(ends = "both", length = unit(0.3, "cm")),
           linewidth=1) +
  annotate("text",
           x      = -Inf, y      = -Inf,
           label  = "Low ent\nlow pow\nhigh dist",
           size=5,
           hjust  = 0, vjust =  1.3,
           color='gray50') +
  annotate("text",
           x      =  Inf, y      = -Inf,
           label  = "High ent\nhigh pow\nlow dist",
           size=5,
           hjust  =  1, vjust =  1.3,
           color='gray50') +
  annotate("segment",
           x    = -Inf, xend =  -Inf,
           y    = -1.5, yend = 3,
           arrow = arrow(ends = "both", length = unit(0.3, "cm")),
           linewidth=1) +
  annotate("text",
           x      = -Inf, y      = Inf,
           label  = "High dist\nhigh pow",
           size=5,
           angle  = 90,
           hjust  = 1, vjust =  -0.5,
           color='gray50') +
  annotate("text",
           x      =  -Inf, y      = -Inf,
           label  = "Low dist\nlow pow",
           size=5,
           angle  = 90,
           hjust  =  -0.1, vjust =  -0.5,
           color='gray50')

cairo_pdf('../../data/08.Regression_for_hyperparameters/07.PCA_plot.pdf',
          width = my_plotting_params[['single_plot_width']]*1.5, height = my_plotting_params[['single_plot_height']]*1.5, family = my_plotting_params[['font']])
print(p)
dev.off()


  







