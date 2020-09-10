##

correct.err.flags  <- function (x, y, x0 = NULL, y0 = NULL, ind = 1:2) {
  if (length(x) == 0 || length(y) == 0)
  {
    oldw <- getOption("warn")
    options(warn = -1)
    rr <- do.call(cbind, list(c(x0, x[1]), c(y0, y[1]))[ind])
    rr <- as.data.frame(na.omit(rr))
    names(rr) <- c("starts", "ends")
    options(warn = oldw)
    return(rr)
  }
  if (x[1]<=y[1])
    correct.err.flags(y, x[-1], y0, c(x0, x[1]), rev(ind))
  else
    correct.err.flags(x, y[-1], x0, y0, ind)
}


## correct.err.flags <- function (starts, ends) {
##   if (starts[1] > ends[1]) {
##     ends <- ends[-1]
##     correct.err.flags(starts, ends)
##   }
##   ind <- seq(min(sapply(list(starts, ends), length)))
##   err.0 <- Position(function (i) starts[i]>ends[i], ind[-1]) + 1
##   err.1 <- Position(function (i) starts[i-1]>ends[i], ind[-1]) + 1
##   err.2 <- Position(function (i) starts[i]<ends[i-1], ind[-1]) + 1
##   if (any(sapply(list(err.0, err.1, err.2), Negate(is.na)))) {
##     ##if (!is.na(err.0))
##       ##ends <- ends[-err.0]
##     ##if (!is.na(err.1))
##       ##starts <- starts[-err.1]
##     ##if (!is.na(err.2))
##       ##ends <- ends[-err.2]
##     ends <- ends[na.omit(-c(err.0, err.1, err.2))]
##     correct.err.flags(starts, ends)
##   } else if (length(starts) != length(ends)) {
##     starts <- starts[ind]
##     ends <- ends[ind]
##   }
##   return (list(starts = starts, ends = ends))
## }
