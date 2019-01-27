bind.popeye <- function(x, variable, FUN)
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
  if(!('popeye' %in% class(x)))
    err <- simpleError('x must be a popeye object...')
  if(exists("err"))
    stop(err)

  # Assignments
  data <- x$data

  # Check whether IA has been labeled in fixation tables
  labeled <- vapply( 
    data,
    function(x){
      r <- vapply(x$fix_data, function(y) iaColName %in% names(y), logical(1))
      if(sum(r) == length(r)) TRUE else FALSE
    },
    logical(1)
  )
  if(sum(labeled) != length(labeled)){
    message("No IA found in one or several subjects and stimuli, fixing: ")
    data <- get.ia(x)
  }

  x$data <- lapply(
    x$data,
    function(currsubj){
      currsubj$fix_data <- lapply(
        currsubj$fix_data,
        function(currstim){
          cbind(currstim, FUN(currstim[[variable]]))
        }
      )
      currsubj
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

  return(x)
}
