fit_lines <- function(
  start_pts,
  fix_data_trial,
  keep_y_var,
  use_run_rule,
  k_bounds,
  o_bounds,
  s_bounds,
  den_sd_cutoff,
  den_ratio_cutoff
){
  # Intitial parameter values (slope, vertical offset, sd)
  init_params = c(0, 0, 0)
  
  # Find the best fitting parameters
  fit <- optim(
    init_params,
    create_lines,
    fix_data = fix_data_trial,
    start_pts = start_pts,
    k_bounds = k_bounds,
    o_bounds = o_bounds,
    s_bounds = s_bounds
  )
  
  # Rerun to get additional information
  line_ret <- create_lines(
    fit$par,
    fit_it = FALSE,
    start_pts = start_pts,
    fix_data = fix_data_trial,
    keep_y_var = keep_y_var,
    use_run_rule = use_run_rule,
    den_sd_cutoff = den_sd_cutoff,
    den_ratio_cutoff = den_ratio_cutoff,
    k_bounds = k_bounds,
    o_bounds = o_bounds,
    s_bounds = s_bounds
  )
  
  # Return
  return(line_ret)
}


