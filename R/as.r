as.popeye <- function(eye.file,
                      aoi.file = NULL,
                      ia_tables_header = FALSE,
                      trial_id = NULL,
                      stimPrefix = "stim_",
                      xy_bounds = NULL,
                      ...
                      ) {
  ## This function aggregates eye tracking data and area of interests into a
  ## unified popeye object. It is designated for single subject analysis.
  ## ARGUMENTS:
  ##   eye.file :: the file name of eye-tracking data of a subject
  ##   aoi.file :: the csv files containing the coordinates of all interest
  ##   areas per trial
  ## RETURN:
  ##   A popeye object

  message("Processing:", eye.file, "... ")

  ## See whether eye file is an ASC file or an fixation report
  file.extension <- substring(eye.file, nchar(eye.file) - 2, nchar(eye.file))
  if (file.extension == "asc")
    data <- read.asc(eye.file, ...)
  else if (file.extension == "txt")
    data <- read.fix.report(eye.file)
  else
    stop("Unrecognized file format, *.asc or *.txt file expected.")
  ## Check the completeness of fixation data
  expect_name <- c("fix_data", "n_trials", "dur_summary") # necessary for fix # aligning
  if (!all(expect_name%in%names(data)))
    stop("Incomplete eye-tracking data.")

  ## Remove first fixation per trial
  data$fix_data <- lapply(data$fix_data, function(x) x[-1, ] )

  ## Remove out-of-bounds
  if (!is.null(xy_bounds))
    data <- within(data, fix_data <- lapply(seq(fix_data), function(i) {
      x <- fix_data[[i]]
      ind <- (x$x > xy_bounds[i, 3] &
              x$x < xy_bounds[i, 4] &
              x$y > xy_bounds[i, 1] &
              x$y < xy_bounds[i, 2])
      x[ind, ]}))

  ## Set epoch numbers
  if (is.null(trial_id))
    data <- within(data, names(fix_data) <- paste0(stimPrefix, 1:n_trials))
  else
    data <- within(data, names(fix_data) <- paste0(stimPrefix, trial_id))

  ## Set AOI
  aoi <- lapply(aoi.file, read.csv, header = ia_tables_header)
  ## Check if the first four columns are (t)op, (b)ottom, (l)eft, (r)ight, or
  ## change them to
  message("Setting the first four columns of IA tables as: Top Bottom Left Right...")
  check.tblr <- function(table, columns = c("t", "b", "l", "r"), ...)
    if (!all(columns %in% names(table))) {
      aoi.arg <- check.three.ellipsis(..., target.arg = "aoi.columns")
      names(table) <- aoi.arg$aoi.columns
      check.tblr(table, columns)}
    else
      table[, order(match(colnames(table), columns))]
  aoi <- lapply(aoi, function(x) check.tblr(x, ...))
  ## Set epoch numbers
  names(aoi) <- names(data$fix_data)

  ## calculate start points based on aoi
  start.points <- lapply(aoi, function(x) {
    ## Discretize lines
    lines <- disc.lines(x$t)
    ## x-coordinates as left limits for each line
    xs <- as.vector(tapply(seq(nrow(x)), lines, function(i) min(x$l[i])))
    ## y-coordinates for each line
    ys <- as.vector(tapply(seq(nrow(x)), lines, function(i) mean((x$t[i]+x$b[i])/2)))
    data.frame(x = xs, y = ys)})

  ## If AOI is not NULL, do fix aligning
  if (!is.null(aoi.file)) {
    data$fix_meta_FA <- lapply(seq(aoi), function(currTrial) {
      ## if fixation aligning arguments missed
      fa.arg <- check.three.ellipsis(
        ...,
        target.arg = c(
          "keep_y_var",
          "use_run_rule",
          "k_bounds",
          "o_bounds",
          "s_bounds",
          "den_sd_cutoff",
          "den_ratio_cutoff"))
      ## call fixation aligning
      do.call(function(...)
        fit.lines(
          ## the AOI must be in TBLR order
          start_pts = start.points[[currTrial]],
          data$fix_data[[currTrial]],
          ...),
        fa.arg)})
    ## Separate fixation data from fix_data_FA out
    data$fix_data_FA <- lapply(data$fix_meta_FA, function(x) x$fix_data)
    data$fix_data_FA <- lapply(data$fix_data_FA, function(x) {
      x$y <- x$y_new
      x$y_new <- NULL
      x})}
  data <- within(data, names(fix_data_FA) <- names(fix_data))

  data$aoi <- aoi
  class(data) <- "popeye"
  return(data)
}

### ----------------------------------------------------------------------------

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
