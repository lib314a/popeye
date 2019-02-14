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

  # Check object type ----------------------------------------------------------

  if(!('popeye' %in% class(x)))
    stop(simpleError('x must be a popeye object...'))

  # Assignments
  data <- x$data

  # Check IA labels ------------------------------------------------------------

  labeled <- vapply( 
    data,
    function(x){

      # Global variable *iaColName already in col names?
      r <- vapply(x, function(y) iaColName %in% names(y), logical(1))
      if(sum(r) == length(r))

        TRUE

      else

        FALSE

    },
    logical(1)
  )

  # If there are unlabeled data...
  if (sum(labeled) != length(labeled))
  
  {
    message("No IA found in one or several subjects and stimuli, fixing: ")
    # ... label them.
    data <- get.ia(x)
  }

  # Bind new column ------------------------------------------------------------
  x$data <- lapply(
    data,
    function(currsubj){
      lapply(
        currsubj,
        function(currstim){
          cbind(currstim, FUN(currstim[[variable]]))
        }
      )
    }
  )

  return(x)
}
