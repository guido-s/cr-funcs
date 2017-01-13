uniqvals <- function(x, cens.name, sort = FALSE) {
  if (is.factor(x))
    res <- levels(x)
  else
    res <- unique(x)
  ##
  if (!missing(cens.name))
    res <- res[res != cens.name]
  ##
  if (sort)
    res <- sort(res)
  ##
  res
}
