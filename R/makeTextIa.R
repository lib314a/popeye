makeTextIa <- function(
  pathToImage
  , gapThre = 1
  , typeImage = 'png'
  , vertExpand = 10
  , horizExpand = 1 # Two ia may overlap if this number is larger than 1
  , typesetting = 'across' # The orientation of texts, either "across" or "down"
){
  # This function makes interest areas (IA) from text images
  # NOTE: It only process images with alpha channel, if none, add one
  # using *addAlphChan function

  # Read in the image as *pict, the image format must be PNG
  if (
    'png' %in% c(tools::file_ext(pathToImage), typeImage)
    && require('png')
  ){
    pict <- png::readPNG(pathToImage)
  }else{
    message('Image format error.')
    return(NULL)
  }

  # Define the interest layer. Usually it's the alpha layer or the sum of RGB
  # layers.
  alph <- pict[, , 4] # The 4th value in 3rd dim is alpha value
  #alph <- apply(pict, c(1,2), mean)
  vertSum <- apply(alph, 1, mean)
  vertdiff <- diff(vertSum < 0.01)
  vertstarpoin <- which(vertdiff == 1)
  vertEndPoin <- which(vertdiff == -1)
  
  # find margins word by word
  l <- lapply(
    1:length(vertstarpoin)
    , function(x){
      m <- alph[vertstarpoin[x]:vertEndPoin[x],]
      horidiff <- diff(apply(m, 2, mean) < 0.1)
      horistarpoin <- which(horidiff == 1)
      horiEndPoin <- which(horidiff == -1)
      # remove areas that have a gap smaller than *gapThre*
      TH <- vapply(
        horistarpoin[-1] - horiEndPoin[-length(horiEndPoin)]
        , function(x){
            if((length(x) > 0)&(x > gapThre)) T else F
        }
        , logical(1)
      )
      horistarpoin <- horistarpoin[c(TRUE, TH)]
      horiEndPoin <- horiEndPoin[c(TH, TRUE)]
      sapply(
        1:length(horistarpoin)
        , function(x) c(horistarpoin[x], horiEndPoin[x])
      )
    }
  )
  
  # return a matrix contain margin values
  lett <- vector(length = 4)
  for (i in 1:length(l)) {
    for (j in 1:ncol(l[[i]])){
      tblr <- c(
        vertstarpoin[i],
        vertEndPoin[i],
        l[[i]][, j]
      )
      lett <- rbind(lett, tblr)
    }
  }
  if(gapThre != 0) resu <- lett[-1, ]
  
  # Enlarge ia to fill gaps
  resu[, 1] <- resu[, 1] + vertExpand
  resu[, 2] <- resu[, 2] - vertExpand
  resu[, 3] <- resu[, 3] + horizExpand
  resu[, 4] <- resu[, 4] - horizExpand

  # Report *resu
  message(paste('The IA coordinates of', pathToImage, 'is:'))
  sapply(capture.output(print(resu)), message)

  return(resu)
}

# order numbers of columns correspond to margins
# +-------------------- 1 ----------------------+
# |  E    EE  EEEEEEE  E       E       EEEEEEE  |
# | EE    EE EE       EE      EE       EE   EE  |
# | EE    EE EE       EE      EE       EE   EE  |
# 3 EEEEEEEE EEEEEEE  EE      EE       EE   EE  4
# | EE    EE EE       EE      EE       EE   EE  |
# | EE    EE EE       EE      EE       EE   EE  |
# | EE    EE EEEEEEEE EEEEEEE EEEEEEE  EEEEEEE  |
# +-------------------- 2 ----------------------+

# *graphics::rect* function has the order: l, b, r, t

