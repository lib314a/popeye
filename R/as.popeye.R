as.popeye <- function(eye_data, back_images, ia_tables, ia_labels = NULL,
  start_flag = 'TRIALID', corrected = FALSE)
{
  # This function concatenates eye tracking data, table of coordinates
  #
  # Args:
  #   eye_data: Either a vector of the names of a 
  #   ia_tables: Either a vector of the names of csv files containing the
  #     coordinates of all interest areas
  #   back_images: Either a vector of the names of... must be raster images
  #   corrected: whether raw/uncorrected eyetracking data or not
  #
  # Return:
  #   An object of popeye class

  # Messages
  mesCED.1 <-
  "The eye data is not in a proper format, please input the correct paths of
  the uncorrected asc files, or input the uncorrected asc data with
  popeye::read.asc.file..." 
  mesCED.2 <-
  "Non-identical trial numbers in eye data, the trial numbers are:"
  mesCED.3 <-
  "Setting the first two columns of eye data as x and y; if no columns named x
  and y are found, the first two columns will be renamed to x and y..." 
  mesCBI.1 <-
  "Please input background images as png..."
  mesCIT.1 <-
  "Setting the first four columns of IA tables as (t)op, (b)ottom, (l)eft,
  (r)ight..." 
  mesCIT.2 <- 
  "Please input csv files as IA tables..."

  # Check eye data -------------------------------------------------------------
  if(is.file(eye_data)) {
    asc <- lapply(eye_data, read.asc.file, start_flag = start_flag)
  }else{
    expect_name <- sort(c( "fix_data", "n_trials", "dur_summary",
      "trial_id_start", "trial_start", "trial_end"))
    asc_name <- sort(names(eye_data))
    if(!identical(expect_name, asc_name))
    {
      message(mesCED.1)
      return(invisible())
    } else {
      asc <- eye_data
    }
  }

  # Check if all items in *asc have the same trial number
  trial.n <- vapply(asc, function(x) x$n_trials, integer(1))
  asc.trial.yes <- (sum(!duplicated(trial.n)) == 1)
  if(!asc.trial.yes) {
    message(mesCED.2)
    sapply(capture.output(trial.n), message)
    return(invisible())
  }

  # Check if the first two columns are x and y coordinates, or change them to
  message(mesCED.3)
  asc <- lapply(
    asc,
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

  # Set subject numbers
  names(asc) <- paste(subjPrefix, 1:length(asc), sep = '')

  # Set stimuli numbers
  asc <- lapply(
    asc,
    function(x){
      within(
        x,
        names(fix_data) <- paste(stimPrefix, 1:n_trials, sep = '')
      )
    }
  )

  # Check backgroud images -----------------------------------------------------
  if(is.file(back_images)){
    # Check file types
    suffices <- vapply(strsplit(back_images, split = '\\.'), function(x)
      x[length(x)], character(1))
    if(sum(suffices == imageType) == length(suffices)) {
      bim <- lapply(back_images, png::readPNG)
    }else{
      message(mesCBI.1)
      return(invisible())
    }
  }else{
    bim <- back_images
  }

  # Set stimuli numbers
  names(bim) <- paste(stimPrefix, 1:length(bim), sep = '')

  # Check ia tables ------------------------------------------------------------
  message(mesCIT.1)

  if(is.file(ia_tables)){
    # Check file types
    suffices <- vapply(
      strsplit(ia_tables, split = '\\.'),
      function(x) x[length(x)],
      character(1)
    )
    if(sum(suffices == tableType) == length(suffices)){
      ia <- lapply(ia_tables, read.csv)
    }else{
      message(mesCIT.1)
      return(invisible())
    }
  }else{
    ia <- ia_tables
  }

  # Check if the first four columns are (t)op, (b)ottom, (l)eft, (r)ight, or
  # change them to 
  ia <- lapply(
    ia,
    function(x){
      if(sum(startIaName %in% colnames(x)) == 4){
        x[, order(match(colnames(x), startIaName))]
      }else{
        colnames(x)[1:4] <- startIaName
        x
      }
    }
  )

  # Set stimuli numbers
  names(ia) <- paste(stimPrefix, 1:length(ia), sep = '')

  # Check if *bim and *ia have the same length (trial number)
  stim.trial.yes <- (length(ia) == length(bim))
  if(!stim.trial.yes){
    message(
      'IA tables and background images have different numbers, the number of IA
      tables is ', length(ia), ', the number of background images is ',
      length(bim)
    )
    return(invisible())
  }

  # Check if trial number equals to lengths of *ia and *bim
  if(trial.n != length(ia)){
    message(
      "The number of trials in eye data doesn't match the number of stimuli.
      There are ", length(ia), " stimuli, while ", trial.n, " trials in eye
      data."
    )
    return(invisible())
  }

  # Check IA labels ------------------------------------------------------------
  if(is.null(ia_labels)) ia_labels <- lapply(ia, function(x) 1:nrow(x))

  # Concatenate eye tracking data, interest areas, and underlay images, and make
  # popeye data
  r <- list(data = asc, interest = ia, underlay = bim, labels = ia_labels, 
    asc_file = lapply(eye_data, readLines), start_flag = start_flag)
  # TODO: Read ASC files from disk rather than store them in the object
  class(r) <- c('popeye', if(!corrected) 'uncorrected' else 'corrected')

  return(r)
}
