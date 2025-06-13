library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)
library(dplyr)
library(lubridate)
library(ggplot2)

library(RColorBrewer)
pal3 <- brewer.pal(3, "Set1")
# "#E41A1C", "#377EB8","#4DAF4A"


simulate_routes <- function(bf, lon_min, lon_max, lat_min, lat_max, season='prebreeding', n_routes=1000){
  if (!(season %in% c('prebreeding', 'postbreeding'))) {
    stop()
  } else {
    dates_ <- bf$dates[bf$dates$timestep %in% BirdFlowR::lookup_season_timesteps(bf, season, season_buffer = 1),]$date
  }
  
  st_dist <- BirdFlowR::get_distr(bf, which=dates_, from_marginals = FALSE)
  xy <- BirdFlowR::i_to_xy(1:nrow(st_dist), bf)
  latlon <- BirdFlowR::xy_to_latlon(bf=bf, xy$x, xy$y)
  valid_points <- latlon[(latlon$lat>=lat_min) & (latlon$lat<=lat_max) & (latlon$lon>=lon_min) & (latlon$lon<=lon_max),]
  st_dist <- st_dist[as.integer(rownames(valid_points)), ]
  st_mean <- colMeans(st_dist)
  st_mean <- as.data.frame(st_mean)
  st_mean$date <- as.Date(dates_)
  
  sim_routes <- BirdFlowR::route(bf, n=n_routes, from_marginals = F)
  sim_data <- sim_routes$data
  sim_data <- sim_data[sim_data$date %in% dates_,]
  sim_mean <- sim_data |> dplyr::group_by(.data[['date']]) |>
    dplyr::summarise(n_indiv = dplyr::n_distinct(
      .data[['route_id']][.data[['lat']] >= lat_min & .data[['lat']] <= lat_max & .data[['lon']] >= lon_min & .data[['lon']] <= lon_max],
      na.rm = TRUE
    ))
  
  return(list(
    sim_mean=sim_mean,
    sim_data=sim_data,
    st_mean=st_mean
  ))
  
}



calculate_daily_route_changes <- function(sim_data, lon_min, lon_max, lat_min, lat_max, season='prebreeding') {
  
  # 1) compute per‐bird‐per‐day inside/enter/leave flags
  route_days <- sim_data |>
    mutate(
      inside_flag = lat >= lat_min & lat <= lat_max &
        lon >= lon_min & lon <= lon_max
    ) |>
    group_by(route_id, date) |>
    summarise(
      inside = any(inside_flag),
      .groups = "drop"
    ) |>
    arrange(route_id, date) |>
    group_by(route_id) |>
    mutate(
      prev_inside = lag(inside, default = FALSE),
      enters     = !prev_inside & inside,   # crossed in
      leaves     =  prev_inside & !inside   # crossed out
    ) |>
    ungroup()
  
  # 2) find which birds never leave
  if (season == "prebreeding") {
    residents <- route_days |>
      group_by(route_id) |>
      summarise(ever_leaves = any(leaves), .groups="drop") |>
      filter(!ever_leaves) |>
      pull(route_id)
  } else if (season == "postbreeding") {
    start_date <- min(route_days$date)
    residents <- route_days |>
      filter(date == start_date, inside) |>
      pull(route_id) |>
      unique()
  } else {
    stop()
  }
  
  # 3) tag “permanent” inside for those residents
  route_days <- route_days |>
    mutate(
      is_resident = route_id %in% residents
    )
  
  # 4) aggregate back up to daily counts
  daily_route_changes <- route_days |>
    group_by(date) |>
    summarise(
      n_enters          = sum(enters),
      n_leaves          = sum(leaves),
      n_staying_any     = sum(inside),                              # your old “temporary stays”
      n_staying_never   = sum(inside & is_resident),                # only those that never leave
      .groups = "drop"
    ) |>
    # (then do your midpoint‐date and scaling if you like)
    mutate(
      date_mid       = as.POSIXct(date) - lubridate::hours(84),
      max_n          = max(n_enters, n_leaves, n_staying_any, n_staying_never),
      enters_scaled  = (n_enters     - min(n_enters))     / (max_n - min(n_enters)),
      leaves_scaled  = (n_leaves     - min(n_leaves))     / (max_n - min(n_leaves)),
      stays_scaled   = (n_staying_never - min(n_staying_never)) / (max_n - min(n_staying_never))
    ) |>
    select(-max_n)
  
  # daily_route_changes <- daily_route_changes[2:nrow(daily_route_changes),]
  return(daily_route_changes)
}




plot_immigration_emigration <- function(daily_route_changes,
                                         file_name='balori_spring_phenology_decomposed.pdf') {
  
  # loess_enters <- loess(
  #   enters_scaled ~ as.numeric(date_mid),
  #   data = daily_route_changes,
  #   span = 0.5
  # )
  # loess_leaves <- loess(
  #   leaves_scaled ~ as.numeric(date_mid),
  #   data = daily_route_changes,
  #   span = 0.8
  # )

  # loess_enters <- loess(
  #   enters_scaled ~ as.numeric(date_mid),
  #   data   = daily_route_changes,
  #   span   = 0.5,
  #   family = "symmetric",           # robust fitting
  #   control= loess.control(surface="direct")
  # )
  # 
  # loess_leaves <- loess(
  #   leaves_scaled ~ as.numeric(date_mid),
  #   data   = daily_route_changes,
  #   span   = 0.5,
  #   family = "symmetric",           # robust fitting
  #   control= loess.control(surface="direct")
  # )

  ss_enters <- smooth.spline(
    x   = as.numeric(daily_route_changes$date_mid),
    y   = daily_route_changes$enters_scaled,
    spar= 0.15
  )
  
  ss_leaves <- smooth.spline(
    x   = as.numeric(daily_route_changes$date_mid),
    y   = daily_route_changes$leaves_scaled,
    spar= 0.15
  )
  
  # loess_stays <- loess(
  #   stays_scaled ~ as.numeric(date_mid),
  #   data = daily_route_changes,
  #   span = 0.2
  # )
  
  numeric_min <- min(as.numeric(daily_route_changes$date_mid))
  numeric_max <- max(as.numeric(daily_route_changes$date_mid))
  grid_numeric <- seq(numeric_min, numeric_max, length.out = 1000)
  tz <- attr(daily_route_changes$date_mid, "tzone")
  
  grid_df <- tibble(
    date_mid = as.POSIXct(grid_numeric, origin = "1970-01-01", tz = tz)
  ) |>
    mutate(
      # enters_smooth = predict(loess_enters, newdata = as.numeric(date_mid)),
      # leaves_smooth = predict(loess_leaves, newdata = as.numeric(date_mid))
      enters_smooth = predict(ss_enters, x = as.numeric(date_mid))$y,
      leaves_smooth = predict(ss_leaves, x = as.numeric(date_mid))$y
    ) |>
    mutate(
      ymin       = pmin(enters_smooth, leaves_smooth),
      ymax       = pmax(enters_smooth, leaves_smooth),
      # fill_color = if_else(enters_smooth >= leaves_smooth, pal3[1], pal3[2]),
      fill_color = factor(if_else(enters_smooth >= leaves_smooth,
                                  "Immigration",
                                  "emigration"),
                          levels = c("Immigration", "emigration")),
      diff = abs(enters_smooth - leaves_smooth)
    ) |>
    filter(diff > 1e-6) |>
    arrange(date_mid) |>
    mutate(
      change_flag = (fill_color != lag(fill_color, default = first(fill_color))),
      segment_id  = cumsum(change_flag)
    )
  
  # dummy_ribbon <- data.frame(
  #   date_mid   = range(grid_df$date_mid),       # min & max date_mid
  #   ymin       = c(0, 0),                       # any y within your panel
  #   ymax       = c(0, 0),                       # zero‐height is fine
  #   fill_color = factor(pal3[3]),
  #   segment_id = 0                              # a dummy group
  # )
  
  p <- ggplot() +
    geom_ribbon(
      data    = grid_df,
      aes(x = date_mid, ymin = ymin, ymax = ymax, fill = fill_color, group = segment_id),
      alpha   = 0.3
    ) +
    # geom_ribbon(
    #   data       = dummy_ribbon,
    #   aes(x       = date_mid,
    #       ymin    = ymin,
    #       ymax    = ymax,
    #       fill    = fill_color,
    #       group   = segment_id),
    #   alpha      = 0,
    #   show.legend= TRUE
    # ) +
    geom_line(
      data = grid_df,
      aes(x = date_mid, y = enters_smooth),
      color = pal3[1], linewidth = 2
    ) +
    geom_line(
      data = grid_df,
      aes(x = date_mid, y = leaves_smooth),
      color = pal3[2], linewidth = 2
    ) +
    # geom_line(
    #   data = grid_df,
    #   aes(x = date_mid, y = stay_smooth),
    #   color = pal3[3], linewidth = 2
    # ) +
    geom_point(
      data = daily_route_changes,
      aes(x = date_mid, y = enters_scaled),
      color = pal3[1], size = 6, alpha = 0.6
    ) +
    geom_point(
      data = daily_route_changes,
      aes(x = date_mid, y = leaves_scaled),
      color = pal3[2], size = 6, alpha = 0.6
    ) +
    # scale_fill_manual(
    #   name   = "BirdFlow\nweekly movement",
    #   values = c(pal3[1]=pal3[1], pal3[2]=pal3[2]),
    #   labels = c("Immigration","emigration"),
    #   breaks = c(pal3[1],pal3[2])
    # ) +
    scale_fill_manual(
      name   = "BirdFlow\nweekly movement",
      values = c("Immigration" = pal3[1],
                 "emigration" = pal3[2]),
      breaks = c("Immigration", "emigration")
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(alpha = 1)
      )
    ) +
    scale_x_datetime(date_labels = "%b %d") +
    labs(
      x     = "Date", # (mid‐point)
      y     = "Movement intensity", #Scaled counts (no centering)
    ) +
    my_plotting_params[['theme']] +
    my_plotting_params[['my_plotting_params']] +
    theme(
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22)
    )
  
  cairo_pdf(glue::glue('../../data/09.Demonstrations/{file_name}'),
            width = my_plotting_params[['single_plot_width']]/1.5, height = my_plotting_params[['single_plot_height']]/3, family = my_plotting_params[['font']])
  print(p)
  dev.off()
  
  return(p)
}


plot_ebird_abundance_and_breeding_stays <- function(daily_route_changes, st_mean, sim_mean,
                                                    file_name='balori_spring_phenology_ST_vs_BF.pdf') {
  
  max1 <- max(sim_mean$n_indiv)
  min1 <- min(sim_mean$n_indiv)
  sim_mean$n_indiv <- (sim_mean$n_indiv - min1)/(max1 - min1)
  daily_route_changes$n_staying_never <- (daily_route_changes$n_staying_never - min1) / (max1 - min1)
  st_mean$st_mean <- (st_mean$st_mean - min(st_mean$st_mean)) / (max(st_mean$st_mean) - min(st_mean$st_mean))
  
  # ggplot() + geom_point(data=sim_mean, aes(x=date, y=scale(n_indiv, center=F, scale=T)),
  #                       color=pal3[1], shape=1, size=10) +
  #   geom_point(data=st_mean, aes(x=date, y=scale(st_mean, center=F, scale=T)),
  #              color=pal3[2], shape=1, size=9)

  p <- ggplot() +
    geom_point(data=st_mean, aes(x=date, y=st_mean), color=pal3[2], size=6, alpha=0.7, fill=pal3[2]) +
    geom_smooth(data=st_mean, aes(x=date, y=st_mean, color = "eBird S&T\nrelative abundance"), method = "loess", se = FALSE, size = 2, span = 0.32) +
    
    geom_point(data=daily_route_changes, aes(x=date, y=n_staying_never), color=pal3[3], size=6, alpha=0.7, fill=pal3[3]) +
    geom_smooth(data=daily_route_changes, aes(x=date, y=n_staying_never, color = "BirdFlow inferred\nbreeder abundance"), method = "loess", se = FALSE, size = 2, span = 0.32) +
    
    scale_color_manual(
      name   = "", 
      breaks = c(
        "eBird S&T\nrelative abundance",
        "BirdFlow inferred\nbreeder abundance"
      ),
      values = c("eBird S&T\nrelative abundance" = pal3[2], "BirdFlow inferred\nbreeder abundance" = pal3[3])
    ) +
    labs(
      x = "Date",
      y = "Scaled abundance"
    ) + 
    my_plotting_params[['theme']] +
    my_plotting_params[['my_plotting_params']] +
    theme(
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22)
    )
  
  cairo_pdf(glue::glue('../../data/09.Demonstrations/{file_name}'),
            width = my_plotting_params[['single_plot_width']]/1.5, height = my_plotting_params[['single_plot_height']]/3, family = my_plotting_params[['font']])
  print(p)
  dev.off()
  
  return(p)
}


plot_routes_and_box <- function(bf, n=20, season='prebreeding',
                        box_lon_min, box_lon_max, box_lat_min, box_lat_max,
                        xmin=1500000, xmax=7725000, ymin=5000000, ymax=14775000
                        ){
  
  ## 
  routes <- BirdFlowR::route(bf, n=n, from_marginals = F, season=season)
  buffer_prop <- 0.2

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
  
  sf_corners <- sf::st_as_sf(corners, coords = c("x", "y"), crs = terra::crs(bf$geom$crs))
  coast <- get_coastline(sf_corners)
  
  library(sf)
  geo_bbox <- st_as_sfc(
    st_bbox(c(xmin = box_lon_min, xmax = box_lon_max,
              ymin = box_lat_min, ymax = box_lat_max),
            crs = 4326)
  )
  map_crs <- st_crs(terra::crs(bf$geom$crs))
  proj_bbox <- st_transform(geo_bbox, crs = map_crs)
  side <- min(diff(c(xmin, xmax)), diff(c(ymin, ymax)))
  center <- c(mean(c(xmin, xmax)), mean(c(ymin, ymax)))
  h <- side/2
  
  sq_coords <- matrix(c(
    center[1] - h, center[2] - h,
    center[1] + h, center[2] - h,
    center[1] + h, center[2] + h,
    center[1] - h, center[2] + h,
    center[1] - h, center[2] - h
  ), ncol = 2, byrow = TRUE)
  
  proj_square <- st_sfc(st_polygon(list(sq_coords)), crs = map_crs)
  
  
  library(ggsci)
  library(scales)
  p <- ggplot2::ggplot(
    data = routes$data,
    ggplot2::aes(x = .data$x, y = .data$y)
  ) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    ) +
    guides(fill = "none", colour = guide_legend(title = "Species")) +
    ggplot2::geom_point(
      ggplot2::aes(
        size = .data$stay_len
      ), color='orange2'
    ) +
    ggplot2::geom_path(
      ggplot2::aes(
        group = .data$route_id
      ),
      linewidth = 1,
      lineend = "round", 
      color='darkolivegreen3'
    ) +
    ggplot2::geom_sf(
      data = coast,
      inherit.aes = FALSE,
      linewidth = 0.4,
      color = grDevices::grey(.5)
    )  + 
    my_plotting_params[['theme']] +
    my_plotting_params[['my_plotting_params']] +
    geom_sf(data = proj_bbox,
            inherit.aes = FALSE,
            fill   = NA,
            color  = "brown2",
            linewidth=2) +
    coord_sf(
      xlim = c(1900000, 7725000),
      ylim = c(6000000, 13775000),
      expand = FALSE
    ) + theme(legend.position = 'none', axis.title.x = element_blank() , axis.title.y = element_blank())
  
  return(p)
  
}
