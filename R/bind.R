##bind <- function(x, ...)
##  UseMethod("bind", x)

bind.aoi <- function(x) {
  stimname <- names(x$fix_data_FA)
  x$fix_data_FA <- sapply(
    simplify = FALSE,
    names(x$fix_data_FA),
    function(i){
      j <- which(names(x$fix_data_FA) == i)
      mapFix2Ia(x$fix_data_FA[[j]], areas = x$aoi[[j]], 1:nrow(x$aoi[[j]]))
    })
  names(x$fix_data_FA) <- stimname

  return(x)
}

bind <- function(x, FUN, v)
{
  # Bind specific measurement to fixation tables
  #
  # Args:
  #   x:
  #   FUN:
  #
  # Return:
  #   The fixation tables with the measurement attached
  #

  # Check object type ----------------------------------------------------------

  if(!('popeye' %in% class(x)))
    stop(simpleError('x must be a popeye object...'))

  # Bind new column ------------------------------------------------------------
  x$fix_data_FA <- lapply(x$fix_data_FA, function(currstim)
    cbind(currstim, FUN(currstim[[v]])))

  return(x)
}
