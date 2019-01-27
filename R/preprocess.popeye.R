preprocess.popeye <- function(
  x,
  xy_bounds = NULL,
  keep_y_var = FALSE,
  use_run_rule = TRUE,
  den_sd_cutoff = Inf,
  den_ratio_cutoff = 1,
  k_bounds = c(-.1, .1),
  o_bounds = c(-50, 50),
  s_bounds = c(1, 20)
){
  # Take an uncorrected popeye object, realign/correct the fixations via the
  # method described in Andrew L. Cohen (2013). This method is modified from the script
  # by Andrew L. Cohen.
  #
  # Args:
  #   x: the uncorrected popeye obejct
  #
  # Return:
  #   A popeye object
  #
  if(!('popeye' %in% class(x)))
    err <- simpleError('x must be a popeye object...')
  else if(!('uncorrected' %in% class(x)))
    err <- simpleError('x must be uncorrected...')
  if(exists("err"))
    stop(err)

  start_flag <- x$start_flag

  # Start points of lines (y means of IAs)
  start_pts <- lapply(
    x$interest,
    function(x){
      cbind(x = min(x[3]), y = apply(x[, 1:2], 1, mean))
    }
  )

  # Correcting
  newFix <- lapply(
    seq(x$data),
    function(i){
      message("Preprocessing: ", i, "...")

      currdata <- x$data[[i]]
      currname <- names(x$data)[i]

      # Initialize list
      line_ret <- list()

      # Go through the trials
      n_trials <- currdata$n_trials
      for (t in 1:n_trials) {
        # Get the data for this trial
        fix_data_trial <- currdata$fix_data[[t]]

        # Mark out-of-bound fixations
        fix_data_trial <- mark_out_of_bounds(
          fix_data_trial,
          xy_bounds,
          t,
          n_trials
        )

        # Find the best-fitting lines
        line_ret[[t]] <- fit_lines(
          start_pts[[t]],
          fix_data_trial,
          keep_y_var,
          use_run_rule,
          k_bounds,
          o_bounds,
          s_bounds,
          den_sd_cutoff,
          den_ratio_cutoff
        )
      }

      return(line_ret)
    }
  )

  asc_file <- lapply(
    seq(newFix),
    function(i){
      oldFix <- x$asc_file[[i]]
      n_trials <- x$data[[i]]$n_trials
      asc_par <- x$data[[i]]

      as.asc(newFix[[i]], oldFix, n_trials, asc_par)
    }
  )
  x$asc_file <- asc_file
  x$data <- lapply(x$asc_file, function(x) extract.asc(x, start_flag))

  # Check if the first two columns are x and y coordinates, or change them to
  x$data <- lapply(
    x$data,
    function(x){
      x$fix_data <- lapply(
        x$fix_data,
        function(y){
          if(sum(startAscName %in% colnames(y)) == 2){
            y[, order(match(colnames(y), startAscName))]
          }else{
            colnames(x)[1:2] <- startAscName
            y
          }
        }
      )
      x
    }
  )

  # Set subject and stimuli numbers
  names(x$data) <- paste(subjPrefix, 1:length(x$data), sep = '')
  x$data <- lapply(
    x$data,
    function(x){
      names(x$fix_data) <- paste(stimPrefix, 1:x$n_trials, sep = '')
      x
    }
  )

  # Change status from uncorrected to corrcted
  class(x)[class(x) == 'uncorrected'] <- 'corrected'
  return(x)
}
