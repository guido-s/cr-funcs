crtrans <- function(states, initial.states, cens.name) {
  ##
  ## Create transition matrix for competing risks model
  ## (input for mvna() and etm() of respective R packages)
  ##
  if (any(initial.states %in% states))
    stop("Values of arguments 'states' and 'initial.states' must be different.")
  ##
  len.s <- length(states)
  len.i <- length(initial.states)
  len <- len.i + len.s
  res <- matrix(FALSE, ncol = len, nrow = len,
                dimnames = list(c(initial.states, states),
                                c(initial.states, states)))
  ##
  ## Possible transitions:
  ## from initial state to any competing risk
  ##
  res[1:len.i, (len.i + 1):(len)] <- TRUE
  ##
  res
}
