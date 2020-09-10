# This function draw rectangle on matrices of raster images
# draw rectangles on a image matrix
drawRastRect <- function(x1, x2, y1, y2, imagmatr, lineweig = 2, value = 1){

    lw <- lineweig
    # line 1
    imagmatr[(x1 - lw):(x2 + lw), (y1 - lw):y1] <- value
    # line 3
    imagmatr[(x1 - lw):x1, (y1 - lw):(y2 + lw)] <- value
    # line 2
    imagmatr[(x1 - lw):(x2 + lw), (y2 + lw):y2] <- value
    # line 4
    imagmatr[x2:(x2 + lw), (y1 - lw):(y2 + lw)] <- value

    return(imagmatr)
}
