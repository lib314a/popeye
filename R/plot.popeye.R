plot.popeye <- function(x, y = NULL, underlay = NULL, epoch = 1, aoi = FALSE)
{
  ## Constants
  xy_buffer = .1
  ps.min = 1 # point size min
  ps.max = 7 # point size max

  if (!is.null(underlay))
  {
    ## Checking underlay formats
    if (class(underlay) == "character" && length(underlay) == 1)
      underlay <- read.underlay(underlay)
    else if (class(underlay) %in% c("matrix", "array"))
      underlay <- underlay
    else
      stop("Illegal underlay format.")
    ## TEMPORARY modify underlay
    ##underlay <- underlay[length(underlay[, 1, 1]):1, ,]
    underlay <- underlay[, , 1] + underlay[, , 2] + underlay[, , 3]
    underlay <- underlay/max(underlay)
    underlay.width <- length(underlay[1, ])
    underlay.height <- length(underlay[, 1])
  } else {
    underlay.width <- 1024
    underlay.height <- 768
  }

  ## The primary fixations data
  d <- x$fix_data[[epoch]]

  ## Point sizes based on duration
  pt_size <- with(
    x,
    ((ps.max-ps.min)/(dur_summary[5]-dur_summary[1]))*(d$dur - dur_summary[1])+ps.min)

  ## Plot
  #old.par <- par(no.readonly = TRUE)
  #par(oma = c(0, 0, 3, 0))
  #layout(c(1, 2))
  plot(
    c(0, underlay.width),
    c(0, underlay.height),
    main = "Original Fixations with Deletions & Classifications",
    xlab = 'x',
    ylab = 'y',
    xlim = c(0, underlay.width),
    ylim = c(underlay.height, 0))
  if (!is.null(underlay))
    rasterImage(underlay, 0, 0, underlay.width, underlay.height)
  axis(1)
  axis(2)

  fixation.colors <- c("green", "blue", "red", "gray")

  ## The primary fixations
  ## Fixations
  points(d$x, d$y, col = fixation.colors[1], cex = pt_size, pch = 1)
  ## Lines for fixation ordering
  points(d$x, d$y, col = fixation.colors[1], pch = 1, type = "l", lty = "dashed")
  ## Fixation numbers
  text(d$x, d$y, cex = .5, labels = 1:nrow(d), col = fixation.colors[1], pos = 1)

  ## The auxiliary fixations data
  aux.ind <- vapply(names(x), function(y) grepl("fix_data_.*", y), logical(1))
  if (any(aux.ind)) {
    aux.d <- lapply(x[aux.ind], function(y) y[[epoch]])
    for (i in seq(aux.d)) {
      currd <- aux.d[[i]]
      fixation.color <-  fixation.colors[i+1]
      ## The fixations re-aligned
      ## Fixations
      points(currd$x, currd$y, col = fixation.color, cex = pt_size, pch = 1)
      ## Fixation numbers
      text(currd$x, currd$y, cex = .5, labels = 1:nrow(d), col = fixation.color, pos = 1)}}

  ## Legends
  legend(
    "bottomleft",
    pch = 1,
    col = fixation.colors[1:(1+sum(aux.ind))],
    legend = c("fix_data", names(x)[aux.ind]))

}
