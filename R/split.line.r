split.line <- function (x) {
  #strsplit(x, "[\t ]")[[1]]
  x <- gsub("\\s+", " ", x)
  x <- strsplit(x, " ")[[1]]
  return(x)
}
