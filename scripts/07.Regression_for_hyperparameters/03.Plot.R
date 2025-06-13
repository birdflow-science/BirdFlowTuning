library(ggplot2)
RF_metrics_summary <- read.csv('../../data/08.Regression_for_hyperparameters/all_metrics.csv')

source('../plotting_params/plotting_params.R')

## 01. plot randomforest fits
RF_regression <- list()
param_count <- 0
for (param_name in c('ent', 'pow', 'dist')) {
  modeled_results_train <- read.csv(glue::glue('../../data/08.Regression_for_hyperparameters/train_y_cv_true_y_pred_df_{param_name}.csv'))
  # modeled_results_test <- read.csv(glue::glue('../../data/08.Regression_for_hyperparameters/test_y_true_y_pred_df_{param_name}.csv'))
  param_count <- param_count+1
  
  p <- ggplot() + 
    # geom_jitter(data=modeled_results_train, aes(x=y_train, y=y_train_pred),
    #             shape = 21, size=3, alpha=0.9,
    #             fill = "steelblue",
    #             color = "black",
    #             stroke = 0.5,
    #             width = 0, height = 0) +
    # geom_smooth(data=modeled_results_train, aes(x=y_train, y=y_train_pred, color='Train'), method='lm', fill='steelblue', alpha=0.2) + 
    geom_jitter(data=modeled_results_train, aes(x=y, y=y_pred),
                  shape = 21, size=3, alpha=0.9,
                  fill = "orange2",
                  color = "black",
                  stroke = 0.5,
                  width = 0, height = 0) +
    geom_smooth(data=modeled_results_train, aes(x=y, y=y_pred), method='lm', color='orange2', fill='orange2', alpha=0.2) +
    # scale_color_manual(
    #   name   = "Phase",
    #   values = c(
    #     # "Train" = "steelblue",
    #     "Train"  = "orange2"
    #   ),
    #   breaks = c("Train") #,"Test"
    # ) +
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
      label = paste0('R\u00B2: ',
                     round(RF_metrics_summary[RF_metrics_summary$hyperparameter==param_name,]$r2, 2)),
      hjust = -2.6,
      vjust = 12,
      fill= NA,
      label.size = 0,
      size=7
    ) +
    my_plotting_params[['theme']] +
    my_plotting_params[['formater']]
  
    print(p)

  cairo_pdf(glue::glue('../../data/08.Regression_for_hyperparameters/0{param_count}.RF_fit_{param_name}.pdf'),
            width = my_plotting_params[['single_plot_width']], height = my_plotting_params[['single_plot_height']], family = my_plotting_params[['font']])
  print(p)
  dev.off()
  
  RF_regression[[length(RF_regression)+1]] <- p
}



## 02. plot correlation: morphology and hyperparameters
validation_summary <- read.csv('../../data/08.Regression_for_hyperparameters/Best_models_by_ST098_and_LL_validation_summaries_with_range_stats.csv')
validation_summary <- validation_summary |> dplyr::group_by(.data[['sp']]) |> dplyr::slice(1)
validation_summary['ent'] = validation_summary['ent']/(validation_summary['ent'] + validation_summary['dist'])
validation_summary['dist'] = validation_summary['dist']/(validation_summary['ent'] + validation_summary['dist'])

library(RColorBrewer)
n <- validation_summary$ORDER1_eBird |> unique() |> length()
pal <- colorRampPalette(brewer.pal(12, "Paired"))(n)

## p1
cor_ = cor(log(validation_summary$Mass), validation_summary$ent, use='complete.obs')
p_mass_ent <- ggplot(data=validation_summary, aes(x=Mass, y=ent, fill=ORDER1_eBird))+
  geom_jitter(shape = 21, size=3, alpha=0.9,
              color = "black",
              stroke = 0.5,
              width = 0, height = 0.01) +
  scale_fill_manual(values = pal, name = "Order") +
  geom_smooth(method='lm', fill='steelblue', color='steelblue', alpha=0.2) +
  scale_x_log10() +
  labs(x='Body mass (g)') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']] +
  annotate(
    "label",
    x     = Inf,
    y     = Inf,
    label = paste0("Pearson's r: ",
                   round(cor_, 2)),
    hjust = 1.2,
    vjust = 3,
    fill= NA,
    label.size = 0,
    size=7
  )
p_mass_ent

cairo_pdf('../../data/08.Regression_for_hyperparameters/04.Mass_ent_regression.pdf',
          width = my_plotting_params[['single_plot_width']]*1.5, height = my_plotting_params[['single_plot_height']]*1.5, family = my_plotting_params[['font']])
print(p_mass_ent)
dev.off()

## p2
cor_ = cor(log(validation_summary$Mass), validation_summary$dist, use='complete.obs')
p_mass_dist <- ggplot(data=validation_summary, aes(x=Mass, y=dist, fill=ORDER1_eBird))+
  geom_jitter(shape = 21, size=3, alpha=0.9,
              color = "black",
              stroke = 0.5,
              width = 0, height = 0.01) +
  scale_fill_manual(values = pal, name = "Order") +
  geom_smooth(method='lm', fill='steelblue', color='steelblue', alpha=0.2) +
  scale_x_log10() +
  labs(x='Body mass (g)') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']] +
  annotate(
    "label",
    x     = Inf,
    y     = Inf,
    label = paste0("Pearson's r: ",
                   round(cor_, 2)),
    hjust = 1.2,
    vjust = 3,
    fill= NA,
    label.size = 0,
    size=7
  )
p_mass_dist

cairo_pdf('../../data/08.Regression_for_hyperparameters/05.Mass_dist_regression.pdf',
          width = my_plotting_params[['single_plot_width']]*1.5, height = my_plotting_params[['single_plot_height']]*1.5, family = my_plotting_params[['font']])
print(p_mass_dist)
dev.off()

## p3
cor_ = cor(log(validation_summary$postbreeding_abundance_variation), validation_summary$pow, use='complete.obs')
p_postbreeding_abundance_variation_pow <- ggplot(data=validation_summary, aes(x=postbreeding_abundance_variation, y=pow, fill=ORDER1_eBird))+
  geom_jitter(shape = 21, size=3, alpha=0.9,
              color = "black",
              stroke = 0.5,
              width = 0, height = 0.01) +
  scale_fill_manual(values = pal, name = "Order") +
  geom_smooth(method='lm', fill='steelblue', color='steelblue', alpha=0.2) +
  scale_x_log10() +
  labs(x='Spatial unevenness of average\nabundance during post-breeding migration') +
  my_plotting_params[['theme']] +
  my_plotting_params[['formater']] +
  annotate(
    "label",
    x     = Inf,
    y     = Inf,
    label = paste0("Pearson's r: ",
                   round(cor_, 2)),
    hjust = 1.2,
    vjust = 3,
    fill= NA,
    label.size = 0,
    size=7
  )
p_postbreeding_abundance_variation_pow

cairo_pdf('../../data/08.Regression_for_hyperparameters/06.postbreeding_abundance_variation_pow_regression.pdf',
          width = my_plotting_params[['single_plot_width']]*1.5, height = my_plotting_params[['single_plot_height']]*1.5, family = my_plotting_params[['font']])
print(p_postbreeding_abundance_variation_pow)
dev.off()


## 03. PCA plot
validation_summary <- read.csv('../../data/08.Regression_for_hyperparameters/Best_models_by_ST098_and_LL_validation_summaries_with_range_stats.csv')
validation_summary <- validation_summary |> dplyr::group_by(.data[['sp']]) |> dplyr::slice(1)
validation_summary['ent'] = validation_summary['ent']/(validation_summary['ent'] + validation_summary['dist'])
validation_summary['dist'] = validation_summary['dist']/(validation_summary['ent'] + validation_summary['dist'])

set.seed(42)
pca_res <- prcomp(validation_summary[,c('ent','dist', 'pow')], 
                  center = TRUE,
                  scale. = TRUE)
pca_res
ss <- summary(pca_res)
print(ss)

new_validation_summary <- cbind(validation_summary, as.data.frame(pca_res$x))



p_pca <- ggplot(data=new_validation_summary, aes(x=PC1, y=PC2, fill=ORDER1_eBird))+
  geom_jitter(shape = 21, size=3, alpha=0.9,
              color = "black",
              stroke = 0.5,
              width = 0.1, height = 0.1) +
  scale_fill_manual(values = pal, name = "Order") +
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
           label  = "Low ent\nhigh dist\nlow pow",
           size=5,
           hjust  = 0, vjust =  1.3,
           color='gray50') +
  annotate("text",
           x      =  Inf, y      = -Inf,
           label  = "High ent\nlow dist\nhigh pow",
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
print(p_pca + my_plotting_params[['formater']])
dev.off()


#### 04. color by body mass
p_pca_but_color_by_mass <- ggplot(data=new_validation_summary, aes(x=PC1, y=PC2, fill=Mass))+
  geom_jitter(shape = 21, size=3, alpha=0.9,
              color = "black",
              stroke = 0.5,
              width = 0.1, height = 0.1) +
  scale_fill_viridis_c(name = "Body mass (g)", trans = "log",
                       breaks = c(20, 150, 1000, 8000),
                       labels = c("20", "150", "1000", "8000")
                       ) +
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
           label  = "High ent\nlow dist\nhigh pow",
           size=5,
           hjust  = 0, vjust =  1.3,
           color='gray50') +
  annotate("text",
           x      =  Inf, y      = -Inf,
           label  = "Low ent\nhigh dist\nlow pow",
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
           label  = "High pow\nlow ent\nhigh dist",
           size=5,
           angle  = 90,
           hjust  = 1, vjust =  -0.5,
           color='gray50') +
  annotate("text",
           x      =  -Inf, y      = -Inf,
           label  = "Low pow\nhigh ent\nlow dist",
           size=5,
           angle  = 90,
           hjust  =  -0.1, vjust =  -0.5,
           color='gray50')

cairo_pdf('../../data/08.Regression_for_hyperparameters/08.PCA_plot_bodymass_colored.pdf',
          width = my_plotting_params[['single_plot_width']]*1.5, height = my_plotting_params[['single_plot_height']]*1.5, family = my_plotting_params[['font']])
print(p_pca_but_color_by_mass)
dev.off()

## Combined patchwork
library(patchwork)
pp1 <- ggplot(data=new_validation_summary, aes(x=PC1, y=PC2, fill=ORDER1_eBird))+
  geom_jitter(shape = 21, size=6, alpha=0.8,
              color = "black",
              stroke = 0.5,
              width = 0.1, height = 0.1) +
  scale_fill_manual(values = pal, name = "Order") +
  ggrepel::geom_text_repel(aes(label = common_name), size = 6, max.overlaps = 6) +
  ggrepel::geom_text_repel(data=new_validation_summary[(new_validation_summary$PC2 < 1) & (new_validation_summary$PC1 < -2),],
                           aes(label = common_name), size = 6, max.overlaps = 23) +
  # ggrepel::geom_text_repel(data=new_validation_summary[new_validation_summary$PC1 < -2,],
  #                          aes(label = common_name), size = 6, max.overlaps = 23) +
  theme(
    panel.background = element_blank(),    # no gray
    panel.grid       = element_blank(),    # no grid
    axis.line        = element_blank(),    # remove default axes
    panel.border     = element_rect(       # add a black border
      colour = "black", 
      fill   = NA, 
      linewidth   = 1
    )
  ) +
  labs(
    x=paste0('PC1 (', round(ss$importance['Proportion of Variance','PC1']*100, 2), '%)'),
    y=paste0('PC2 (', round(ss$importance['Proportion of Variance','PC2']*100, 2), '%)')
  ) +
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
           label  = "Low ent\nhigh dist\nlow pow",
           size=10,
           hjust  = 0, vjust =  1.3,
           color='gray50') +
  annotate("text",
           x      =  Inf, y      = -Inf,
           label  = "High ent\nlow dist\nhigh pow",
           size=10,
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
           size=10,
           angle  = 90,
           hjust  = 1, vjust =  -0.5,
           color='gray50') +
  annotate("text",
           x      =  -Inf, y      = -Inf,
           label  = "Low dist\nlow pow",
           size=10,
           angle  = 90,
           hjust  =  -0.1, vjust =  -0.5,
           color='gray50')+ 
  theme(aspect.ratio = 1,
                     axis.title.x = element_text(size = 35),
                     axis.title.y = element_text(size = 35, margin = margin(r = 20)),
                     axis.text.x = element_text(size = 25),
                     axis.text.y = element_text(size = 25),
                     legend.title = element_text(size = 30, face = "bold"),
                     legend.text = element_text(size = 25),
                     text = element_text(size = 25),
        legend.position="bottom"
        ) +
  scale_fill_manual(
    values = pal,
    name   = "Order",
    guide  = guide_legend(
      override.aes = list(
        shape  = 21,     # match your geom_jitter(shape=21)
        size   = 6,      # size of the dots *in the legend*
        stroke = 0.5     # border width for those filled circles
      ),
      keywidth  = unit(1.2, "cm"),
      keyheight = unit(1.2, "cm"),
      ncol      = 2     # if you want them stacked
    )
  )


# pp1_plus <- plot_spacer() / pp1 / plot_spacer() + plot_layout(heights = c(,1,0.5))
pp2 <- (RF_regression[[1]] + p_mass_ent + theme(legend.position = "none", plot.margin = unit(c(10, 10, 0, 1), "cm")) + plot_layout(ncol = 2))/
  (RF_regression[[2]] + p_postbreeding_abundance_variation_pow + theme(legend.position = "none", plot.margin = unit(c(0, 10, 0, 1), "cm")) + plot_layout(ncol = 2))/
  (RF_regression[[3]] + p_mass_dist + theme(legend.position = "none", plot.margin = unit(c(0, 10, 10, 1), "cm")) + plot_layout(ncol = 2))

pp <- pp1 + theme(plot.margin = unit(c(0, 0, 20, 5), "cm")) + theme(
  legend.position      = c(0.5, -0.3), # 10% of panel height below
  legend.justification = c(0.5, 1),    # anchor the legend’s top center
  legend.direction     = "horizontal"
) + pp2 + theme(plot.margin = unit(c(10, 10, 10, 0), "cm")) + plot_layout(ncol = 2, widths = c(0.3,0.5)) + 
  plot_annotation(tag_levels = list(c('(a)','(b)','(e)','(c)','(f)', '(d)', '(g)'))) & 
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.tag.position  = c(0.02, 0.98)
  )

cairo_pdf('../../data/08.Regression_for_hyperparameters/08.ALL_COMBINED.pdf',
          width = my_plotting_params[['single_plot_width']]*3, height = my_plotting_params[['single_plot_height']]*3, family = my_plotting_params[['font']])
print(pp)
dev.off()


