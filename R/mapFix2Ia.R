mapFix2Ia <- function(fix, areas, labels)
{
  # This function combines fixations and interest areas into a long data frame,
  # so that the each fixation could be classified into predefined types (first 
  # pass, regression, etc...) in the next step.
  #
  # Args:
  #   fix: a matrix or data frame that contains the coordinates of fixation
  #     positions. Lines stand for individual fixations, columns contains
  #     coordinates. The coordinates should be arranged at a pattern of (x, y)
  #   areas: a matrix or data frame that contains the coordinates of interest
  #     areas. Lines stand for areas, columns contains coordinates. The
  #     coordinates should be arranged at a pattern of tblr (top, bottom, left,
  #     right)
  #   labels: denotes the name or index of the areas. It could be a vector of
  #     integers or characters, but will always be converted to a character
  #
  # Returns:
  #   A data frame of n+1 columns: n is the number of fix tables, plus the
  #   serial/line number of the corresponding IA.

  ## if no complete fixation in a trial
  if (is.null(fix)) {
    message("Empty trial found and skipped...")
    return (fix)
  }
  if (nrow(fix) == 0) {
    message("Empty trial found and skipped...")
    return (fix)
  }
  if (nrow(fix[!is.na(fix$x) & !is.na(fix$x), ]) == 0) {
    message("Empty trial found and skipped...")
    return (fix)
  }

  # If *fix is a vector (single fixation), transpose it
  if(is.vector(fix))
    fix   <- t(fix)
  # If *areas is a vector (single interest area), transpose it
  if(is.vector(areas))
    areas <- t(areas)

  # Map fixation to interest areas
  r.labels  <- apply(fix, 1, function(x){

      fix.x <- as.integer(x[colnames(fix)=='x'])
      fix.y <- as.integer(x[colnames(fix)=='y'])
      ia.t  <- as.integer(areas[, 1])
      ia.b  <- as.integer(areas[, 2])
      ia.l  <- as.integer(areas[, 3])
      ia.r  <- as.integer(areas[, 4])

      # Get ia index
      ind   <- which(ia.l<=fix.x & ia.r>=fix.x & ia.b>=fix.y & ia.t<=fix.y)
      # If *ind is a null value, return NA
      if(is.null(ind) || (length(ind)==0))
        ind <- NA
      # Pick the first TRUE value if there are more than one
      else if(length(ind) > 1)
        ind <- ind[1]

      # Get label
      r.lab <- labels[ind]
      # If *r.lab is not single valued, return NA
      if(length(r.lab)!=1)
        r.lab <- NA

      # Convert the result to character
      return(as.character(r.lab))
    }
  )

  # Returns
  r <- cbind(fix, r.labels)

  if (iaColName%in%names(r))
    iaColName <- paste0(iaColName, ".new")
  names(r)[ncol(r)] <- iaColName
  return(r)
}
