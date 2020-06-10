read.asc <- function(asc_lines, start_flag)
{
  # Constants
  col_range <- 5:8

  # Read in the file
  f <- readLines(asc_lines)

  # Get trial start and end
  trial_id_start <- which(grepl('TRIALID', f))
  trial_start <- which(grepl(start_flag, f))
  trial_end <- which(grepl('END', f))

  # Handle situations where two trials starts are not
  # separated by trial ends (e.g., due to aborted trials).
  # Get rid of first one.

  # Get rid of errant TRIALIDs
  marked_starts <- numeric()
  start_index <- 1
  end_index <- 1
  while (start_index < length(trial_id_start)) {
    if (trial_id_start[start_index + 1] < trial_end[end_index]) {
      marked_starts <- c(marked_starts, start_index)
    } else {
      end_index <- end_index + 1
    }
    start_index <- start_index + 1
  }

  if (length(marked_starts) > 0)
    trial_id_start <- trial_id_start[-marked_starts]

  # Get rid of errant start_flags
  marked_flags <- numeric()
  flag_index <- 1
  end_index <- 1
  while (flag_index < length(trial_id_start)) {
    if (trial_start[flag_index + 1] < trial_end[end_index]) {
      marked_flags <- c(marked_flags, flag_index)
    } else {
      end_index <- end_index + 1
    }
    flag_index <- flag_index + 1
  }

  if (length(marked_flags) > 0)
    trial_start <- trial_start[-marked_flags]

  # Number of trials
  n_trials <- length(trial_start)

  # Initialize fixation data
  fix_data <- list()

  # Keep duration data in a vector to get summary info
  all_dur <- numeric(0)

  # Analyze each trial
  for (t in 1:n_trials) {
    # Data for one trial
    trial_data  <- f[trial_id_start[t]:trial_end[t]]

    # Trial start time ----------------------------------------------------------- Bing
    trial_start_t <- trial_data[1]
    trial_start_t <- gsub('\\s+', ' ', trial_start_t)
    trial_start_t <- strsplit(trial_start_t, ' ')
    trial_start_t <- as.numeric(trial_start_t[[1]][2])

    # Find the start and end of the fixations
    trial_start_fix <- which(grepl('SFIX', trial_data))
    trial_end_fix <- which(grepl('EFIX', trial_data))

    # Handle situations where two start fixations are not
    # separated by end fixations (e.g., due to track loss).
    # Get rid of first one.
    marked_starts <- numeric()
    start_index <- 1
    end_index <- 1
    while (start_index < length(trial_start_fix)) {
      if (trial_start_fix[start_index + 1] < trial_end_fix[end_index]) {
        marked_starts <- c(marked_starts, start_index)
      } else {
        end_index <- end_index + 1
      }
      start_index <- start_index + 1
    }

    if(length(marked_starts) > 0) # -------------------------------------------- Bing
      trial_start_fix <- trial_start_fix[-marked_starts]

    # Onsets for fixations ----------------------------------------------------- Bing
    onsets <- trial_data[trial_start_fix]
    onsets <- gsub('\\s+', ' ', onsets)
    onsets <- strsplit(onsets, ' ')
    onsets <- vapply(onsets, function(x) as.numeric(x[3]) - trial_start_t, numeric(1))

    # Fixations for a trial and get rid of extra white space
    trial_fix <- trial_data[trial_end_fix]
    trial_fix <- gsub('\\s+', ' ', trial_fix)
    trial_fix <- strsplit(trial_fix, ' ')

    # Fixation duration, location, pupil size
    n_fix <- length(trial_fix)

    # n_fixations x 4 (duration, x, y, pupil_size)
    fix_data_trial <- matrix(NA, nrow = n_fix, ncol = 4)

    for (i in 1:n_fix) {
      fix_data_trial[i, 1:4] <- as.numeric(trial_fix[[i]][col_range])
    }

    fix_data_trial <- data.frame(fix_data_trial)
    names(fix_data_trial) <- c('dur', 'x', 'y', 'pupil')

    # Originally mark all fixations as keepers
    fix_data_trial$type = 'keep'

    # Mark any fixations before the start flag as not in trial (NIT)
    fix_data_trial$type[trial_end_fix < (trial_start[t] - trial_id_start[t] + 1)] <-
      'nit'

    # Find the first full fixation after the start flag, mark others as partial (part)
    fix_data_trial$type[trial_start_fix < (trial_start[t] - trial_id_start[t] + 1) &
                          trial_end_fix > (trial_start[t] - trial_id_start[t] + 1)] <-
      'part'

    # Add onset col ------------------------------------------------------------ Bing
    fix_data_trial$onset <- onsets

    # Store duration information
    all_dur <- append(all_dur, fix_data_trial$dur)

    fix_data[[t]] <- fix_data_trial

  }

  # Put all data together in one list
  asc_data <- list()

  asc_data$fix_data <- fix_data
  asc_data$n_trials <- n_trials
  asc_data$dur_summary <- fivenum(all_dur)

  asc_data$trial_id_start <- trial_id_start
  asc_data$trial_start <- trial_start
  asc_data$trial_end <- trial_end

  return(asc_data)

}

### ----------------------------------------------------------------------------

read.fix.report <- function(file) {
  ## Read in the file
  f <- read.table(file, sep = "\t", header = T)

  ## Number of trials
  n_trials <- length(unique(f$TRIAL_INDEX))

  ## fixation data
  fix_data <- list()

  for (currTrial in unique(f$TRIAL_INDEX))
    fix_data[[paste0("t_", as.character(currTrial))]] <-
      with(f,
        data.frame(
          dur = CURRENT_FIX_DURATION[TRIAL_INDEX == currTrial]
        , x = CURRENT_FIX_X[TRIAL_INDEX == currTrial]
        , y = CURRENT_FIX_Y[TRIAL_INDEX == currTrial]
        , pupil = CURRENT_FIX_PUPIL[TRIAL_INDEX == currTrial]
        , type = "keep"))

  ## Put all data together in one list
  asc_data <- list()
  asc_data$fix_data <- fix_data
  asc_data$n_trials <- n_trials
  asc_data$dur_summary <- fivenum(unlist(lapply(fix_data, function(x) x$dur)))

  return(asc_data)
}

### ----------------------------------------------------------------------------

read.underlay <- function(file)
{
  nl <- strsplit(file, split = "\\.")[[1]]
  last <- nl[length(nl)]
  if (last == "png")
    png::readPNG(file)
  else if (last == "tiff")
    tiff::readTIFF(file)
  else
    stop("Non-supported image format.")
}
