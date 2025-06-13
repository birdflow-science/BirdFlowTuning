library(ggplot2)

my_plotting_params <- list()

## 00. plot size
my_plotting_params[['single_plot_width']] <- 10
my_plotting_params[['single_plot_height']] <- 10
my_plotting_params[['font']] <- 'Arial'

## 01.theme
my_plotting_params[['theme']] <- theme(
  panel.background = element_blank(),    # no gray
  panel.grid       = element_blank(),    # no grid
  axis.line        = element_blank(),    # remove default axes
  panel.border     = element_rect(       # add a black border
    colour = "black", 
    fill   = NA, 
    linewidth   = 1
  ),
  axis.title.x = element_text(size = 24),
  axis.title.y = element_text(size = 24),
  axis.text.x = element_text(size = 17),
  axis.text.y = element_text(size = 17),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 16)
)



my_plotting_params[['formater']] <- ggh4x::force_panelsizes(rows = unit(5, "in"),
                          cols = unit(5, "in"))

my_plotting_params[['formater_long_plot']] <- ggh4x::force_panelsizes(rows = unit(10, "in"),
                                                            cols = unit(5, "in"))


## 02. HV line
hvline_color <- "red2"
zero_vline <- geom_vline(
  xintercept = 0,
  color      = hvline_color,
  linetype   = "dashed"
)
zero_hline <- geom_hline(
  yintercept = 0,
  color      = hvline_color,
  linetype   = "dashed"
)
my_plotting_params[['hvline_color']] <- hvline_color
my_plotting_params[['zero_vline']] <- zero_vline
my_plotting_params[['zero_hline']] <- zero_hline

## 03. scatter plot
my_plotting_params[['scatter']] <- geom_point(shape = 21, size=3, alpha=0.9,
           fill = "steelblue",
           color = "black",
           stroke = 0.5)

my_plotting_params[['scatter_with_xjitter']] <- geom_jitter(shape = 21, size=3, alpha=0.9,
                                                           fill = "steelblue",
                                                           color = "black",
                                                           stroke = 0.5,
                                                           width = 0.1, height = 0)

