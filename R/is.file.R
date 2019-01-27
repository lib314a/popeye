is.file <- function(filenames)
{
  # Tests if the first argument is referring to a file
  #
  # Return:
  #   TRUE or FALSE

  r <- vapply(
    filenames,
    function(x)
      if(class(filenames) == 'character'){
        r <- file.exists(filenames)
        if(length(r) == sum(r))
          TRUE
        else
          FALSE
      }else{
        FALSE
      }
    , logical(1)
  )

  if(sum(r) == length(r))
    return(TRUE)
  else
    return(FALSE)
}
