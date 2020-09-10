count.starting.false <- function(x, n = 0)
  ifelse(!x[1], count.starting.false(x[-1], n+1), n)

count.trailing.false <- function(x, n = 0)
  ifelse(!x[length(x)], count.trailing.false(x[-length(x)], n+1), n)

## find exactly one anchor in binary vector x, by eliminate sided FALSE and
## compute the middle char of the rest logical values

find.starting.anchor <- function(x) {
  if (length(x)==1) return(x)
  if (all(x==FALSE)) return(x)
  anchor.ind <- count.starting.false(x)+1
  re <- rep(FALSE, length(x))
  re[ceiling(anchor.ind)] <- TRUE
  re
}

find.middle.anchor <- function(x) {
  if (length(x)==1) return(x)
  if (all(x==FALSE)) return(x)
  anchor.ind <- (count.starting.false(x)+length(x)-count.trailing.false(x))/2
  re <- rep(FALSE, length(x))
  re[ceiling(anchor.ind)] <- TRUE
  re
}

find.ending.anchor <- function(x) {
  if (length(x)==1) return(x)
  if (all(x==FALSE)) return(x)
  anchor.ind <- (length(x) - count.trailing.false(x))
  re <- rep(FALSE, length(x))
  re[ceiling(anchor.ind)] <- TRUE
  re
}

## find all anchors in a binary vector x of a line, and return the anchors as
## binary vector
find.anchors <- function(chunking.factor,
                              exclusions,
                              anchor.position = c("middle", "starting", "ending"))
{
  apply.exclusions <- function(footage, exclusions) {
    if (!is.list(exclusions) || !all(sapply(exclusions, is.logical)))
      stop("Exclusions for anchors must be a list of logical vectors!")
    if (length(exclusions) <1)
      return(footage)
    else {
      footage[exclusions[[1]]] <- TRUE
      apply.exclusions(footage, exclusions[-1])
    }
  }

  exclusion.vector <- apply.exclusions(rep(FALSE, length(chunking.factor)), exclusions)
  exclusion.vector[is.na(chunking.factor)] <- TRUE
  ## invert binary vector as TRUE represents information and FALSE noise
  footage.vector <- !exclusion.vector

  if (is.character(anchor.position))
    anchor.position <- switch(
      anchor.position,
      "middle" = find.middle.anchor,
      "starting" = find.starting.anchor,
      "ending" = find.ending.anchor)

  ## find exactly one anchor in binary vector x, by eliminate sided FALSE and
  ## compute the middle char of the rest logical values
  splits <- c(0, which(exclusion.vector), length(exclusion.vector)+1)
  re <- rep(FALSE, length(chunking.factor))
  for (i in seq(splits[-1])) {
    range <- (splits[i]+1):(splits[i+1]-1)
    currclause <- chunking.factor[range]
    currbinary <- footage.vector[range]
    ## take care of ending punctuation
    if (NA%in%currclause) next
    ##
    re[range] <- unlist(
      lapply(split(currbinary, currclause), anchor.position))
  }
  re
}
