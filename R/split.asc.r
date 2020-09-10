split.asc <- function (lines, start.flag, end.flag = NULL) {
  starts <- grep(start.flag, lines)
  if (is.null(end.flag))
    ends <- c(starts[-1], length(lines))
  else
    ends <- grep(end.flag, lines)
  x <- correct.err.flags(starts, ends)
  starts <- x$starts
  ends <- x$ends
  r <- lapply(lisp::zip.c(starts, ends), function (x) lines[x[1]:x[2]])
  names(r) <- lines[starts]
  return(r)
}
