setchar <- function(x, val, text, list = FALSE, name = NULL) {
  ##
  ## Auxiliary function (from R package meta)
  ##
  if (is.null(name))
    name <- deparse(substitute(x))
  nval <- length(val)
  ##
  idx <- charmatch(tolower(x), tolower(val), nomatch = NA)
  ##
  if (any(is.na(idx)) || any(idx == 0)) {
    if (list)
      first <- "List element '"
    else
      first <- "Argument '"
    ##
    if (missing(text)) {
      if (nval == 1)
        vlist <- paste('"', val, '"', sep = "")
      else if (nval == 2)
        vlist <- paste('"', val, '"', collapse = " or ", sep = "")
      else
        vlist <- paste(paste('"', val[-nval],
                             '"', collapse = ", ", sep = ""),
                       ', or ', '"', val[nval], '"', sep = "")
      ##
      stop(first, name, "' should be ", vlist, ".",
           call. = FALSE)
    }
    else
      stop(first, name, "' ", text, ".", call. = FALSE)
  }
  ##
  val[idx]
}
