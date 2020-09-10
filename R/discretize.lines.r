## simple discretization using convolution, the input is the y coordinates of
## either top or bottom coordinates of AOI, and is smoothed by convolving with a
## window of 1, which yields the safe ranges of lines.

disc.lines <- function(x, win.width = 5) {
  start<- min(x)-win.width
  stop <- max(x)+win.width
  axis <- rep(0, stop-start+1)
  axis[unique(x-start)] <- 1
  axis <- convolve(axis, rep(1, win.width), type = "o") > .5

  ## the start and end points for categorize y coordinates
  groups.starts <- start+which(diff(axis)==1)+1
  groups.ends   <- start+which(diff(axis)==-1)-1

### calculate the y coordinates of the start points directly
##  sapply(
##    seq(groups.starts),
##    function(i) (groups.starts[i]+groups.ends[i])/2)

### calculate line indexes
  vapply(x, function(y) which((groups.starts<=y)&(groups.ends>=y)), integer(1))
}
