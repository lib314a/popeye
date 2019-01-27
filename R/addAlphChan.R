addAlphChan <- function(pArray, value)
{
  # This function adds alpha channel to a png pict, i.e. appends a 4th matrix to
  # the array of the picture with a specified value (0 for totally transparency,
  # and 1 for totally vague)
  f <- dim(pArray) # Row, column, and layer

  # Report
  message(
    paste(
      "The raster image has the dimension of: "
      , capture.output(print(f)))
      , sep = ''
  )

  # If an picture has 2 or 4 layers, it's already alpharized, return the
  # original image
  if(length(f) == 3) if(f[3] %in% c(2, 4)){
    message("This picture already has an alpha channel!")
    return(pArray)
  }else if(!(length(f) %in% c(2, 3))){
  # If it has layers numbers other than 2 or 3, it is not a png image. Return
  # NULL, stopping it from further processing
    message("NOT an array of raster image, returning NULL...")
    return(NULL)
  }

  # Produce the alpha channel
  valSeq <- rep(value, f[1]*f[2]) # transparent(1) <---> vague(0)
  alphChan <- matrix(valSeq, nrow = f[1], ncol = f[2])

  # Bind alpha channel to the rater image
  r <- abind::abind(pArray, alphChan, along = 3)
  return(r)
}
