create_lines <- function(
  params,
  fix_data,
  start_pts,
  k_bounds,
  o_bounds,
  s_bounds,
  fit_it = TRUE,
  keep_y_var,
  use_run_rule,
  den_sd_cutoff,
  den_ratio_cutoff
){
  # fit_it: TRUE -> return fit measure, FALSE -> return fit information
  # den_sd_cutoff: remove points for which the density is > this many sd away from mean density
  # den_ratio_cutoff: remove points for which (max density)/(2nd max density) not high enough
  
  # Unpack the parameters
  if (is.null(k_bounds))
    k <- 0
  else
    k <- k_bounds[1] + (k_bounds[2] - k_bounds[1]) * pnorm(params[1])
  
  if (is.null(o_bounds))
    o <- 0
  else
    o <- o_bounds[1] + (o_bounds[2] - o_bounds[1]) * pnorm(params[2])
  
  s <- s_bounds[1] + (s_bounds[2] - s_bounds[1]) * pnorm(params[3])
  
  # The y-values for the lines
  ys <- start_pts[, 2]
  
  # The number of clusters is based off of the lines
  n_clusters <- length(ys)
  
  # Initialize some matrices
  data_den <- matrix(numeric(0), nrow(fix_data), n_clusters)
  y_diff <- matrix(numeric(0), nrow(fix_data), n_clusters)
  
  for (l in 1:n_clusters) {
    # The value of each point on each line
    y_on_line <- o + k * (fix_data$x - start_pts[l, 1]) + start_pts[l, 2]
    
    # Log density value for each point based on the line and sd
    data_den[, l] <- log(dnorm(fix_data$y, mean = y_on_line, sd = s))
    
    # Store the difference between the real and fitted value
    y_diff[, l] <- fix_data$y - y_on_line
    
  }
  
  # Find max density line for each point
  # Assume all-or-none classification
  data_den_max <- apply(data_den, 1, max)
  
  # The sum of the log densitities is the fit measure
  # Only use valid, in-bounds fixations
  
  fit_measure <- -sum(data_den_max[fix_data$type == 'keep'])
  
  if (fit_it) {
    # In case log density goes to infinity
    if (fit_measure == Inf)
      fit_measure = .Machine$integer.max
    
    # Return the fit measure
    return(fit_measure)
    
  } else {
    # Mark ambigous points
    data_den_sort <- t(apply(exp(data_den), 1, sort))
    data_den_ratio <-
      data_den_sort[, n_clusters] / data_den_sort[, n_clusters - 1]
    
    ambig_rm <- data_den_ratio < den_ratio_cutoff
    ambig_rm <-
      ambig_rm & fix_data$type != 'oob' & fix_data$type != 'part' &
      fix_data$type != 'nit'
    
    fix_data$type[ambig_rm] <- 'amb'
    
    # Mark points with very low density
    inv_dnorm <- function(x) {
      sqrt(-2 * log(sqrt(2 * pi) * x))
    }
    density_rm <-
      inv_dnorm(exp(data_den_max)) > den_sd_cutoff
    
    # Old way to remove outliers
    #density_rm <-
    #	data_den_max < (mean(data_den_max) - den_sd_cutoff*sd(data_den_max))
    
    density_rm <-
      density_rm & fix_data$type != 'oob' & fix_data$type != 'amb' &
      fix_data$type != 'part' & fix_data$type != 'nit'
    
    fix_data$type[density_rm] <- 'den'
    
    # Category membership
    cats <- apply(data_den, 1, which.max)
    fix_data$cat <- cats
    
    # Reclassify ambiguous pts based on surrounding fixations
    if (use_run_rule) {
      # Get indices of ambiguous pts
      amb_ind <- which(fix_data$type == 'amb')
      
      # Go through each of these points
      for (i in amb_ind) {
        # Go backwards to get category membership of previous keeper
        j = i - 1
        repeat {
          if (j <= 0)
            prev_cat = -1
          else if (fix_data$type[j] == 'keep')
            prev_cat = fix_data$cat[j]
          else if (fix_data$type[j] == 'oob')
            prev_cat = -1
          else if (fix_data$type[j] == 'den')
            prev_cat = -1
          else if (fix_data$type[j] == 'part')
            prev_cat = -1
          else if (fix_data$type[j] == 'nit')
            prev_cat = -1
          else if (fix_data$type[j] == 'amb') {
            j = j - 1
            next
          }
          
          break
          
        }
        
        # Go forwards to get category membership of next keeper
        j = i + 1
        repeat {
          if (j > length(fix_data$type))
            next_cat = -1
          else if (fix_data$type[j] == 'keep')
            next_cat = fix_data$cat[j]
          else if (fix_data$type[j] == 'oob')
            next_cat = -1
          else if (fix_data$type[j] == 'den')
            next_cat = -1
          else if (fix_data$type[j] == 'part')
            # Shouldn't happen, but...
            next_cat = -1
          else if (fix_data$type[j] == 'nit')
            # Shouldn't happen, but...
            next_cat = -1
          else if (fix_data$type[j] == 'amb') {
            j = j + 1
            next
          }
          
          break
          
        }
        
        # If both before and after are from the same category, reclassify
        if (prev_cat == next_cat && prev_cat != -1) {
          fix_data$type[i] = 'keep'
          fix_data$cat[i] = prev_cat
          
        }
        
      }
      
      # Store the new category memberships
      cats <- fix_data$cat
      
    }
    
    # Store recategorized y-values
    if (keep_y_var)
      
      for (i in 1:length(cats))
        fix_data$y_new[i] <- ys[cats[i]] + y_diff[i, cats[i]]
    
    else
      fix_data$y_new <- ys[cats]
    
    # Store category membership, untransformed parameters, fit measure, fixation data
    line_ret <- list()
    
    line_ret$cats <- cats
    line_ret$params <- c(k, o, s)
    line_ret$fit_measure <- fit_measure
    line_ret$fix_data <- fix_data
    line_ret$start_pts <- start_pts
    
    # Return it
    return(line_ret)
  }
  
}

