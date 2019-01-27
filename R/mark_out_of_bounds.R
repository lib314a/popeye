mark_out_of_bounds <- function(fix_data_trial, xy_bounds, t, n_trials)
{
  # If we use bounds
  if (!is.null(xy_bounds)) {
    # Bounds for this trial
    if (is.null(nrow(xy_bounds)) || nrow(xy_bounds) == 1)
      
      xy_bounds_trial <- xy_bounds
    
    else {
      # Make sure there are enough xy_bounds entries
      if (nrow(xy_bounds) != n_trials)
        stop('Length of xy_bounds doesn\'t match number of trials.')
      else
        xy_bounds_trial <- xy_bounds[t, ]
      
    }
    
    # Mark out-of-bounds fixations
    x_keepers <-
      fix_data_trial$x > xy_bounds_trial[1] &
      fix_data_trial$x < xy_bounds_trial[2]
    y_keepers <-
      fix_data_trial$y > xy_bounds_trial[3] &
      fix_data_trial$y < xy_bounds_trial[4]
    
    # Give partial & not in trial fixations priority for marking
    nit <- fix_data_trial$type == 'nit'
    part <- fix_data_trial$type == 'part'
    
    # Mark as out-of-bounds
    fix_data_trial$type[!(x_keepers &
                            y_keepers) & !part & !nit] <- 'oob'
    
  }
  
  # Return it
  return(fix_data_trial)
  
}
