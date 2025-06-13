library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/02.Summarize_validation_preliminary/')
source('load_data_functions.R')
source('../plotting_params/plotting_params.R')

all_res <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
all_res <- all_res |> dplyr::group_by(.data[['sp']], .data[['method']]) |> dplyr::slice(1) |> dplyr::ungroup()
all_res <- all_res[all_res$method=='ST098_and_LL',]
parent_path <- parent_path

## make all maps
all_sp_map <- list()
for (sp in all_res$sp |> unique()) {
  model_name <- all_res[all_res$sp==sp,]$model
  model_path <- glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model_name}')
  bf <- BirdFlowR::import_birdflow(model_path)
  routes <- BirdFlowR::route(bf, n=20, from_marginals = F, season='prebreeding')
  p <- plot_routes(routes) + labs(title = bf$species$common_name, subtitle = bf$species$species_code)
  all_sp_map[[glue::glue('{sp}')]] <- p
}

ncol <- 4
n_row <- ifelse(length(all_sp_map) %% ncol>0, length(all_sp_map) %/% ncol+1, length(all_sp_map) %/% ncol)
pdf(glue::glue("../../data/09.Demonstrations/all_sp_map_facet.pdf"), width = 6*ncol, height = 4*n_row)  # Adjust PDF dimensions as needed
gridExtra::grid.arrange(grobs = all_sp_map, ncol = ncol)  # Adjust 'ncol' to control the number of plots per row
dev.off()

## make stacked map for some species
target_species <- c('balori', 'margod', 'ovenbi1', 'rusbla', 'rufhum', 'wesmea',
                    'gocspa', 'coohaw', 'canwar') #'bkhgro', #, 'woothr', 'amewoo', 

all_sim_routes <- list()
all_routes_combined <- list()
for (sp in target_species) {
  model_name <- all_res[all_res$sp==sp,]$model
  full_model_name <- glue::glue('{parent_path}/{sp}/{sp}_150km/{model_name}')
  model <- import_birdflow(full_model_name)
  routes <- route(model, n=10, season='prebreeding')
  all_sim_routes[[sp]] <- routes
  routes_data <- routes$data
  routes_data$sp <- model$species$common_name
  routes_data$route_id <- paste0(routes_data$route_id, '_', sp)
  all_routes_combined[[sp]] <- routes_data
}

all_routes_combined <- do.call(rbind, all_routes_combined)

## 
buffer_prop <- 0.2
xmin <- ymin <- Inf
xmax <- ymax <-  -Inf
for (route_name in names(all_sim_routes)) {
  the_route <- all_sim_routes[[route_name]]
  xmin <- min(min(routes$data$x), xmin)
  xmax <- max(max(routes$data$x), xmax)
  ymin <- min(min(routes$data$y), ymin)
  ymax <- max(max(routes$data$y), ymax)
}

xbuffer <- (xmax - xmin) * buffer_prop
ybuffer <- (ymax - ymin) * buffer_prop
xmin <- xmin - xbuffer*5
xmax <- xmax + xbuffer*1
ymin <- ymin - ybuffer*5
ymax <- ymax + ybuffer*5

corners <- data.frame(
  x = c(xmin, xmax, xmax, xmin),
  y = c(ymin, ymin, ymax, ymax)
)

sf_corners <- sf::st_as_sf(corners, coords = c("x", "y"), crs = terra::crs(model$geom$crs))
coast <- get_coastline(sf_corners)


library(ggsci)
library(scales)
p <- ggplot2::ggplot(
  data = all_routes_combined,
  ggplot2::aes(x = .data$x, y = .data$y, colour = .data$sp)
) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  ) +
  guides(fill = "none", colour = guide_legend(title = "Species")) +
  ggplot2::geom_point(
    ggplot2::aes(
      size = .data$stay_len
    ),
  ) +
  ggplot2::geom_path(
    ggplot2::aes(
      group = .data$route_id
    ),
    linewidth = 0.85,
    lineend = "round"
  ) +
  ggplot2::geom_sf(
    data = coast,
    inherit.aes = FALSE,
    linewidth = 0.4,
    color = grDevices::grey(.5)
  )  + 
  scale_colour_npg(name = "Species") +
  labs(size = "Stay length\n(week)") +
  my_plotting_params[['theme']] +
  my_plotting_params[['my_plotting_params']] +
  coord_sf(
    xlim = c(1500000, 8725000),
    ylim = c(5000000, 14775000),
    expand = FALSE
  )

## routes
cairo_pdf(glue::glue('../../data/09.Demonstrations/all_stack_map.pdf'),
          width = my_plotting_params[['single_plot_width']]*1, height = my_plotting_params[['single_plot_height']]*1, family = my_plotting_params[['font']])
print(p)
dev.off()


## make stacked map for all species
target_species <- all_res$sp |> unique() #'bkhgro', #, 'woothr', 'amewoo', 

all_sim_routes <- list()
all_routes_combined <- list()
sp_count <- 0
for (sp in target_species) {
  sp_count <- sp_count + 1
  print(sp_count)
  model_name <- all_res[all_res$sp==sp,]$model
  full_model_name <- glue::glue('{parent_path}/{sp}/{sp}_150km/{model_name}')
  model <- import_birdflow(full_model_name)
  routes <- route(model, n=10, season='prebreeding')
  all_sim_routes[[sp]] <- routes
  routes_data <- routes$data
  routes_data$sp <- model$species$common_name
  routes_data$route_id <- paste0(routes_data$route_id, '_', sp)
  all_routes_combined[[sp]] <- routes_data
}

all_routes_combined <- do.call(rbind, all_routes_combined)

## 
buffer_prop <- 0.2
xmin <- ymin <- Inf
xmax <- ymax <-  -Inf
for (route_name in names(all_sim_routes)) {
  the_route <- all_sim_routes[[route_name]]
  xmin <- min(min(routes$data$x), xmin)
  xmax <- max(max(routes$data$x), xmax)
  ymin <- min(min(routes$data$y), ymin)
  ymax <- max(max(routes$data$y), ymax)
}

xbuffer <- (xmax - xmin) * buffer_prop
ybuffer <- (ymax - ymin) * buffer_prop
xmin <- xmin - xbuffer*5
xmax <- xmax + xbuffer*1
ymin <- ymin - ybuffer*5
ymax <- ymax + ybuffer*5

corners <- data.frame(
  x = c(xmin, xmax, xmax, xmin),
  y = c(ymin, ymin, ymax, ymax)
)

sf_corners <- sf::st_as_sf(corners, coords = c("x", "y"), crs = terra::crs(model$geom$crs))
coast <- get_coastline(sf_corners)


library(ggsci)
library(scales)
p <- ggplot2::ggplot(
  data = all_routes_combined,
  ggplot2::aes(x = .data$x, y = .data$y) #, colour = .data$sp
) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  ) +
  guides(fill = "none", colour = guide_legend(title = "Species")) +
  # ggplot2::geom_point(
  #   ggplot2::aes(
  #     size = .data$stay_len / 10
  #   ),
  # ) +
  ggplot2::geom_path(
    ggplot2::aes(
      group = .data$route_id
    ),
    linewidth = 0.85,
    lineend = "round",
    alpha=0.1,
    color='darkgreen'
  ) +
  ggplot2::geom_sf(
    data = coast,
    inherit.aes = FALSE,
    linewidth = 0.4,
    color = grDevices::grey(.5)
  )  + 
  scale_colour_npg(name = "Species") +
  labs(size = "Stay length\n(week)") +
  my_plotting_params[['theme']] +
  my_plotting_params[['my_plotting_params']] +
  coord_sf(
    xlim = c(1500000, 8725000),
    ylim = c(5000000, 14775000),
    expand = FALSE
  )

## routes
cairo_pdf(glue::glue('../../data/09.Demonstrations/all_stack_map_all_species.pdf'),
          width = my_plotting_params[['single_plot_width']]*1, height = my_plotting_params[['single_plot_height']]*1, family = my_plotting_params[['font']])
print(p)
dev.off()



