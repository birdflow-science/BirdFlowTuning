##
library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

load_all("/home/yc85_illinois_edu/BirdFlowPipeline")
load_all("/home/yc85_illinois_edu/BirdFlowR")

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/08.Demonstration_map/')
source('../02.Summarize_validation_preliminary/load_data_functions.R')
source('../plotting_params/plotting_params.R')
source('./phenology_utils.R')

################################################ 01. Load data
all_res <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
all_res <- all_res |> dplyr::group_by(.data[['sp']], .data[['method']]) |> dplyr::slice(1) |> dplyr::ungroup()
tmp <- all_res[all_res['method']=='ST098_and_LL',] ## ST090_and_LL as model selection method

################################################ 02. sp 1: balori
sp <- 'balori'
model_name <- tmp[tmp$sp==sp,]$model
model_path <- glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model_name}')
bf <- BirdFlowR::import_birdflow(model_path)

lon_min <- -92.548990
lon_max <- -86.004801
lat_min <- 37.411889
lat_max <- 40.613238

## prebreeding
res <- simulate_routes(bf, lon_min, lon_max, lat_min, lat_max, season='prebreeding', n_routes=10000)
sim_mean <- res[['sim_mean']]
st_mean <- res[['st_mean']]
sim_data <- res[['sim_data']]
daily_route_changes <- calculate_daily_route_changes(sim_data, lon_min, lon_max, lat_min, lat_max, season='prebreeding')
p21 <- plot_immigration_emigration(daily_route_changes[-1,], file_name=glue::glue('{sp}_spring_phenology_decomposed.pdf')) # remove the first row where the "rate" calculation is an artifact
p11 <- plot_ebird_abundance_and_breeding_stays(daily_route_changes, st_mean, sim_mean,
                                               file_name=glue::glue('{sp}_spring_phenology_ST_vs_BF.pdf'))

## postbreeding
res <- simulate_routes(bf, lon_min, lon_max, lat_min, lat_max, season='postbreeding', n_routes=10000)
sim_mean <- res[['sim_mean']]
st_mean <- res[['st_mean']]
sim_data <- res[['sim_data']]
daily_route_changes <- calculate_daily_route_changes(sim_data, lon_min, lon_max, lat_min, lat_max, season='postbreeding')
p22 <- plot_immigration_emigration(daily_route_changes[-1,], file_name=glue::glue('{sp}_fall_phenology_decomposed.pdf'))  # remove the first row where the "rate" calculation is an artifact
p12 <- plot_ebird_abundance_and_breeding_stays(daily_route_changes, st_mean, sim_mean,
                                               file_name=glue::glue('{sp}_fall_phenology_ST_vs_BF.pdf'))

## routes
p_route1 <- plot_routes_and_box(bf, n=20, season='prebreeding',
                         box_lon_min=lon_min, box_lon_max=lon_max, box_lat_min=lat_min, box_lat_max=lat_max)
cairo_pdf(glue::glue('../../data/09.Demonstrations/{sp}_routes.pdf'),
          width = my_plotting_params[['single_plot_width']]/1.5, height = my_plotting_params[['single_plot_height']]/2, family = my_plotting_params[['font']])
print(p_route1)
dev.off()



################################################ 02. sp 2: margod
sp <- 'margod'
model_name <- tmp[tmp$sp==sp,]$model
model_path <- glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model_name}')
bf <- BirdFlowR::import_birdflow(model_path)

lon_min <- -118.595500
lon_max <- -106.635702
lat_min <- 35.347995
lat_max <- 39.361709

## prebreeding
res <- simulate_routes(bf, lon_min, lon_max, lat_min, lat_max, season='prebreeding', n_routes=10000)
sim_mean <- res[['sim_mean']]
st_mean <- res[['st_mean']]
sim_data <- res[['sim_data']]
daily_route_changes <- calculate_daily_route_changes(sim_data, lon_min, lon_max, lat_min, lat_max, season='prebreeding')
p23 <- plot_immigration_emigration(daily_route_changes[-1,], file_name=glue::glue('{sp}_spring_phenology_decomposed.pdf')) # remove the first row where the "rate" calculation is an artifact
p13 <- plot_ebird_abundance_and_breeding_stays(daily_route_changes, st_mean, sim_mean,
                                               file_name=glue::glue('{sp}_spring_phenology_ST_vs_BF.pdf'))

## postbreeding
res <- simulate_routes(bf, lon_min, lon_max, lat_min, lat_max, season='postbreeding', n_routes=10000)
sim_mean <- res[['sim_mean']]
st_mean <- res[['st_mean']]
sim_data <- res[['sim_data']]
daily_route_changes <- calculate_daily_route_changes(sim_data, lon_min, lon_max, lat_min, lat_max, season='postbreeding')
p24 <- plot_immigration_emigration(daily_route_changes[-1,], file_name=glue::glue('{sp}_fall_phenology_decomposed.pdf'))  # remove the first row where the "rate" calculation is an artifact
p14 <- plot_ebird_abundance_and_breeding_stays(daily_route_changes, st_mean, sim_mean,
                                               file_name=glue::glue('{sp}_fall_phenology_ST_vs_BF.pdf'))

## routes
p_route2 <- plot_routes_and_box(bf, n=20, season='prebreeding',
                                box_lon_min=lon_min, box_lon_max=lon_max, box_lat_min=lat_min, box_lat_max=lat_max)
cairo_pdf(glue::glue('../../data/09.Demonstrations/{sp}_routes.pdf'),
          width = my_plotting_params[['single_plot_width']]/1.5, height = my_plotting_params[['single_plot_height']]/2, family = my_plotting_params[['font']])
print(p_route2)
dev.off()


## Combine them
library(patchwork)
img_arr <- png::readPNG("../../data/09.Demonstrations/balori_photo.png")
img_grob <- grid::rasterGrob(img_arr, interpolate = TRUE)
img_plot1 <- ggplot() +
  annotation_custom(img_grob,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +theme_void()
img_arr <- png::readPNG("../../data/09.Demonstrations/margod_photo.png")
img_grob <- grid::rasterGrob(img_arr, interpolate = TRUE)
img_plot2 <- ggplot() +
  annotation_custom(img_grob,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +theme_void()


pp1 <- p_route1 + img_plot1 + p_route2 + img_plot2 + plot_layout(ncol = 4, widths = c(1, 0.8, 1, 0.8))
pp2 <- (p11 + theme(legend.position = "none") + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + p12 + theme(legend.position = "none") + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + p13 + theme(legend.position = "none") + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + p14 + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))) + plot_layout(ncol = 4)
pp3 <- (p21 + theme(legend.position = "none") + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+ p22 + theme(legend.position = "none") + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + p23 + theme(legend.position = "none") + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + p24 + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))) + plot_layout(ncol = 4)
pp <- pp1 / pp2 /pp3
pp <- pp + plot_layout(heights = c(1.5, 1, 1))

# (RF_regression[[1]] + p_mass_ent + theme(legend.position = "none"))/(RF_regression[[2]] + p_postbreeding_abundance_variation_pow)/(RF_regression[[3]] + p_mass_dist + theme(legend.position = "none"))
# pp <- pp + plot_annotation(tag_levels = list(c('(b)', '(c)', '(g)', '(h)', '(d)', '(e)', '(i)', '(j)'))) & 
#   theme(
#     plot.tag = element_text(size = 25, face = "bold"),
#     plot.tag.position  = c(0.02, 0.98)
#   )

pp <- pp + plot_annotation(tag_levels = list(c('(a)','','(f)','','(b)', '(c)', '(g)', '(h)', '(d)', '(e)', '(i)', '(j)'))) & 
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.tag.position  = c(0.01, 1)
  )

cairo_pdf('../../data/09.Demonstrations/phenology_COMBINED.pdf',
          width = my_plotting_params[['single_plot_width']]*2.5/1.1, height = my_plotting_params[['single_plot_height']]*1.5/1.1, family = my_plotting_params[['font']])
print(pp)
dev.off()





