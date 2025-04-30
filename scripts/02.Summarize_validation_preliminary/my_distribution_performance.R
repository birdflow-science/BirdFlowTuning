
my_distribution_performance <- function(x, log=FALSE, ...) {
  
  stopifnot(inherits(x, "BirdFlow"))
  
  
  ### Transition code
  if (!has_dynamic_mask(x))
    x <- add_dynamic_mask(x)
  
  # Transitions to evaluate over
  transitions <- lookup_transitions(x, ...)
  timesteps <- lookup_timestep_sequence(x, ...)
  start <- timesteps[1]
  end <- timesteps[length(timesteps)]

  #
  start_distr <- cbind(get_distr(x, start, from_marginals = FALSE), # st_
                       get_distr(x, start, from_marginals = TRUE))  # md_
  
  end_distr <- get_distr(x, end, from_marginals = FALSE)
  projected <- predict(x, distr =  start_distr, start =  start,
                       end =  end, direction =  "forward")
  
  projected <- projected[, , dim(projected)[3]] # subset to last timestep
  end_dm <- get_dynamic_mask(x, end)
  
  if (log) {

    md_traverse_cor <- cor(log(end_distr[end_dm]), log(projected[end_dm, 2]))
    return(list(log(end_distr[end_dm]), log(projected[end_dm, 2])))
  } else {

    md_traverse_cor <- cor(end_distr[end_dm], projected[end_dm, 2])
    return(list(end_distr[end_dm], projected[end_dm, 2]))
  }
  
}


end_traverse_xy_data <- function() {
  
}





