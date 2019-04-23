preprocess.popeye <- function(
  x
  , bind_ia = TRUE
  , xy_bounds = NULL
  , keep_y_var = FALSE
  , use_run_rule = TRUE
  , den_sd_cutoff = Inf
  , den_ratio_cutoff = 1
  , k_bounds = c(-.1, .1)
  , o_bounds = c(-50, 50)
  , s_bounds = c(1, 20)
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
      as.matrix(cbind(x = min(x[3]), y = apply(x[, 1:2], 1, mean)))
    }
  )

  s.subj <- names(x$data)
  s.stim <- lapply(s.subj, function(i) names(x$data[[i]]))
  names(s.stim) <- s.subj

  # Go through subjects --------------------------------------------------------
  for (currsubj in s.subj)
  {
    currdata <- x$data[[currsubj]]
    currstim <- s.stim[[currsubj]]
    currmeta <- x$meta[[currsubj]]

    message("Aligning fixations: ", currsubj, "...")

    # Go through the trials ----------------------------------------------------
    line_ret <- list() # Initialize list
    n_trials <- length(currstim)

    for (t in 1:n_trials) {

      message(" - ", currstim[[t]])

      # Get the data for this trial
      fix_data_trial <- currdata[[t]]

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

    oldFile <- x$asc_file[[which(s.subj%in%currsubj)]]
    newFile <- line_ret

    # Update asc file
    x$asc_file[[which(s.subj%in%currsubj)]] <- as.asc(
        newFile
      , oldFile
      , n_trials
      , currmeta
    )

    # Update data
    newFix <- extract.asc(x$asc_file[[which(s.subj%in%currsubj)]], start_flag)
    newFix <- newFix$fix_data
    names(newFix) <- currstim
    x$data[[currsubj]] <- newFix

  }

  names(x$asc_file) <- s.subj

  # Bind IA --------------------------------------------------------------------
  if (bind_ia && ('IA.no'%in%class(x)))
  {
    x$data <- get.ia(x)
    class(x)[class(x) == 'IA.no'] <- 'IA.yes'
  }

  # Change status from uncorrected to corrcted
  class(x)[class(x) == 'uncorrected'] <- 'corrected'
  return(x)
}
