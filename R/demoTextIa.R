# Demonstrate interest areas
# Only works with pngs

demoTextIA <- function(
  foldInput       # The input folder
  , foldOutput    # The output folder
  , gapThres = 1  # Set thres = 1 for single words, 3 for sentences
  , ia = NULL     # If not NULL, it must be a list
  , lineweig = 1
){

  # Concatenate the files in the input folder
  fileInput <- file.path(foldInput, dir(foldInput))
  # Make sure the output folder was there
  if(!file.exists(foldOutput)) dir.create(foldOutput)

  # Define ia list. If *ia is null, replot ia
  if(is.null(ia)){
    iaList <- lapply(
      1:length(fileInput)
      , function(x){
        pict <- fileInput[[x]]
        makeTextIa(pict, gapThres)
      }
    )
  }else{
    iaList <- ia
  }

  for(currimag in fileInput){
    # The image and its dimensions
    pict     <- png::readPNG(currimag)
    dimP     <- dim(pict)
    pictHeig <- dimP[1]
    pictWid  <- dimP[2]
    # Which channel to draw on?
    targChan <- 1:3

    # Make sure the image has an alpha channel
    pict <- addAlphChan(pict, value = 1)

    count <- which(fileInput == currimag)
    for (i in 1:nrow(iaList[[count]])) {
      currline <- iaList[[which(fileInput == currimag)]][i,]
      # Draw to the alpha channel
      for (j in targChan){
        pict[,, j] <- drawRastRect(
          currline[1], currline[2], currline[3], currline[4], pict[,, j],
          lineweig, value = 0
        )
      }
    }
    pict <- pict[,, 1:3]

    # Open a graphic device ----------------------------------------------------
    png(
      filename = file.path(foldOutput, paste(
        'ia', which(fileInput == currimag), '.png', sep = ''
      ))
      , width = pictWid
      , height = pictHeig
    )
    # Draw *pict to the graphic device
    plot(c(0, pictWid), c(0, pictHeig), main = strsplit(currimag, split = '/')[[1]][2])
    rasterImage(pict, 0, 0, pictWid, pictHeig)
    # Add ia indices
    text(
      y = sapply(
        1:nrow(iaList[[count]])
        , function(x){
          pictHeig - mean(iaList[[count]][x,1:2]) # y axis flipped
        }
      )
      , x = sapply(
        1:nrow(iaList[[count]])
        , function(x){
          mean(iaList[[count]][x,3:4])
        }
      )
      , labels = 1:nrow(iaList[[count]])
      , col = 'red'
      , pos = 1
    )
    # Close the graphic device -------------------------------------------------
    dev.off()
  }

  return(iaList)
}
