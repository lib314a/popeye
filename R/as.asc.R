as.asc <- function(
  line_ret,
  orig_asc,
  n_trials,
  asc_data
){
  f <- orig_asc

  # Find all of the EFIX lines after start_flag, put into 1 vector
  trial_id_start <- asc_data$trial_id_start
  trial_start <- asc_data$trial_start
  trial_end <- asc_data$trial_end
  
  efix_lines <- numeric(0)
  for (t in 1:n_trials) {
    trial_data  <- f[trial_id_start[t]:trial_end[t]]
    
    trial_end_fix <- which(grepl('EFIX', trial_data))
    
    efix_lines <-
      append(efix_lines, trial_end_fix + trial_id_start[t] - 1)
    
  }
  
  # Make a vector of all of the y-values in line_ret
  line_ret_y_vals <- numeric(0)
  for (t in 1:n_trials) {
    # Get a single trial of data
    line_ret_trial <- line_ret[[t]]
    
    # Mark the y-values for the deleted fixations
    line_ret_trial$fix_data$y_new[line_ret_trial$fix_data$type == 'nit'] <-
      -1000
    line_ret_trial$fix_data$y_new[line_ret_trial$fix_data$type == 'part'] <-
      -1001
    line_ret_trial$fix_data$y_new[line_ret_trial$fix_data$type == 'oob'] <-
      -1002
    line_ret_trial$fix_data$y_new[line_ret_trial$fix_data$type == 'amb'] <-
      -1003
    line_ret_trial$fix_data$y_new[line_ret_trial$fix_data$type == 'den'] <-
      -1004
    
    # Add the y-values onto the end of the vector
    line_ret_y_vals <-
      append(line_ret_y_vals, line_ret_trial$fix_data$y_new)
    
  }
  
  # Go through each fixation
  k <- 1
  for (i in efix_lines) {
    # Parse out the fixation
    fix_data <- gsub('\\s+', ' ', f[i])
    fix_data <- strsplit(fix_data, ' ')
    
    # What is the next reformatted y-coordinate
    next_y <- line_ret_y_vals[k]
    
    # Change y-coordinate
    fix_data[[1]][7] <- next_y
    
    # Overwrite the original line
    f[i] <- paste(fix_data[[1]], sep = '', collapse = '   ')

    k <- k + 1
  }

  return(f)
}
