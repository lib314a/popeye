find.time <- function (x) {
  r <- Find(function (y) !is.na(as.integer(y)), split.line(x))
  as.integer(r)
}
