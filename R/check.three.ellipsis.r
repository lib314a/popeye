check.three.ellipsis <- function(..., target.arg)
{
  source.arg <- list(...)
  missing.arg <- target.arg[!target.arg %in% names(source.arg)]

  if (length(missing.arg != 0))
    for (i in missing.arg)
    {
      r <- readline(paste0(
        "Argument ", i,
        " missed, please tell me what it should be in your mind:\n",
        "R expression > "
      ))

      source.arg[[i]] <- eval(parse(text = r))
    }

  return(source.arg[target.arg])
}
