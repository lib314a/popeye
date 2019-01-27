get.ia <- function(x, subj = NULL, trial = NULL)
{
  # Align fixations to areas of interest, on a popeye object
  #
  # Args:
  #   x: an popeye object
  #   subj: the subject to be extracted
  #   trial: the trial(s) to be extracted
  #
  # Return:
  #   A data frame of corresponding subjects and trials extracted

  data <- x$data
  ia   <- x$interest
  lab  <- x$labels

  # Check if *x is a popeye object
  if(!('popeye' %in% class(x))){
    message("Not a popeye object.")
    return(invisible())
  }

  # Check if subjects specified
  if(is.null(subj))
    subj  <- 1:length(data)
  else
    subj  <- subj

  # Check if trials specified
  if(is.null(trial))
    trial <- 1:length(ia)
  else
    trial <- trial

  # Extract subjects and trials from *x
  # Map fixations to corresponding IA
  r <- lapply(
    subj,
    function(i){
      message("Subject: ", i, "...")
      r.df <- lapply(
        trial,
        function(j){
          fix <- data[[i]][['fix_data']][[j]]
          ia  <- ia[[j]]
          lab <- lab[[j]]

          message("Trial: ", j, "...")
          #sapply(capture.output(apply(fix, 2, class)), message)
          #sapply(capture.output(apply(ia, 2, class)), message)
          #sapply(capture.output(class(lab)), message)

          mapFix2Ia(fix, ia, lab)
        }
      )
      names(r.df) <- names(data[[i]][['fix_data']])
      r.df
    }
  )

  names(r) <- names(data)
  return(r)
}
