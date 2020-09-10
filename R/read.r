## Fixations are recorded as start by SFIX and end by EFIX
get.fixations <- function (x, col.range = 5:8) {
  message("Position: ", x[1])
  t.trial.start <- find.time(x[1])
  t.trial.end <- find.time(x[length(x)])
  fix <- correct.err.flags(grep("SFIX", x), grep("EFIX", x))
  ## When fixations lose totally
  if (nrow(fix) == 0) {
    message("No fixation found in this trial!")
    return(data.frame(dur = NA, x = NA, y = NA, pupil = NA, onset = NA, type = NA))
  }
  fix.starts <- fix$starts
  fix.ends <- fix$ends
  t.fix.starts <- sapply(x[fix.starts], find.time)
  t.fix.ends <- sapply(x[fix.ends], find.time)
  ## get duration, coordinates and pupil sizes values of the fixation
  v.fix <- sapply(fix.ends, function (i) as.numeric(split.line(x[i])[col.range]))
  v.fix <- as.data.frame(t(v.fix))
  names(v.fix) <- c('dur', 'x', 'y', 'pupil')
  v.fix$onset <- t.fix.starts - t.trial.start
  ## Originally mark all fixations as keepers
  v.fix$type <- 'keep'
  ## Mark any fixations before the start flag as not in trial (NIT)
  v.fix$type[t.fix.ends < t.trial.start] <- "nit"
  ## Find the first full fixation after the start flag, mark others as partial
  ## (part)
  v.fix$type[t.fix.starts<t.trial.start & t.fix.ends>t.trial.start] <- "part"
  v.fix
}

read.asc <- function(asc.lines, start_flag = "START", exclude.trials = NULL, ncore = 3) {
  ## Read in the file
  f <- readLines(asc.lines)
  ## Get trial start and end
  trial.flags <- correct.err.flags(grep("TRIALID", f), grep("END", f))
  trial.id.start <- trial.flags$starts
  trial.end <- trial.flags$ends
  ## remove trials listed in exclude.trials
  if (!is.null(exclude.trials) && length(exclude.trials)!=0) {
    trial.id.start <- trial.id.start[-exclude.trials]
    trial.end <- trial.end[-exclude.trials]}
  fix.data <- lapply(
    lapply(seq(trial.end), function (i) f[trial.id.start[i]:trial.end[i]]),
    get.fixations)
  ## Put all data together in one list
  asc_data <- list()
  asc_data$fix_data <- fix.data
  asc_data$n_trials <- length(fix.data)
  asc_data$dur_summary <- fivenum(unlist(lapply(fix.data, function (y) y$dur)))
  asc_data$trial.id.start <- trial.id.start
  asc_data$trial.end <- trial.end
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
