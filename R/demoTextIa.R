# Demonstrate interest areas
# Only works with pngs

demoTextIA <- function(
  input.file
  , output.folder = "./"   # The output folder
  , lineweig = 1
  , ia = NULL     # If not NULL, it must be a list
  , gapThres = 1  # Set thres = 1 for single words, 3 for sentences
){
  output.file.prefix <- strsplit(input.file, split = "\\/")[[1]]
  output.file.prefix <- output.file.prefix[length(output.file.prefix)]
  output.file.prefix <- strsplit(output.file.prefix, split = "\\.")[[1]]
  output.file.prefix <- paste(output.file.prefix[-length(output.file.prefix)], collapse = ".")
  output.file <- file.path(output.folder, paste( output.file.prefix, "_ia", '.png', sep = ''))

  # Make sure the output folder exists
  if (!file.exists(output.folder)) dir.create(output.folder)

  # If ia is null, replot ia
  if (is.null(ia))
    ia <- makeTextIa(input.file, gapThres = gapThres)

  # The image and its dimensions
  pict     <- png::readPNG(input.file)
  dimP     <- dim(pict)
  pictHeig <- dimP[1]
  pictWid  <- dimP[2]
  # Which channel to draw on?
  targChan <- 1:3

  # Make sure the image has an alpha channel
  pict <- addAlphChan(pict, value = 1)

  count <- which(input.file == input.file)
  for (i in 1:nrow(ia)) {
    currline <- ia[i,]
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

  png(filename = output.file, width = pictWid , height = pictHeig)
  # Draw *pict to the graphic device
  plot(c(0, pictWid), c(0, pictHeig), main = strsplit(input.file, split = '/')[[1]][2])
  rasterImage(pict, 0, 0, pictWid, pictHeig)
  # Add ia indices
  text(
    y = sapply(
      1:nrow(ia)
      , function(x){
        pictHeig - mean(ia[x,1:2]) # y axis flipped
      }
    )
    , x = sapply(
      1:nrow(ia)
      , function(x){
        mean(ia[x,3:4])
      }
    )
    , labels = 1:nrow(ia)
    , col = 'red'
    , pos = 1
  )
  # Close the graphic device -------------------------------------------------
  dev.off()

  return(ia)
}
