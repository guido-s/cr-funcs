crci <- function(x, from, to,
                 timepoints = x$time, level = 0.95) {
  ##
  ## Calculate confidence intervals for cumulative incidences
  ##
  if (!inherits(x, "etm"))
    stop("Argument 'x' must be of class 'etm'.")
  ##
  if (level <= 0 | level >= 1) 
    stop("no valid level for confidence interval")
  alpha <- 1 - level
  ##
  state.names <- rownames(x$est[, , 1])
  from <- setchar(from, state.names)
  to   <- setchar(to, state.names)
  ##
  P   <- trprob(x, paste(from, to), timepoints)
  var <-  trcov(x, paste(from, to), timepoints)
  ##
  se <- sqrt(var) / ((1 - P) * log(1 - P))
  ##
  lower <- 1 - (1 - P)^exp(+ qnorm(1 - alpha / 2) * se)
  upper <- 1 - (1 - P)^exp(- qnorm(1 - alpha / 2) * se)
  ##
  res <- data.frame(from, to,
                    start = x$s, stop = timepoints,
                    P, lower, upper)
  attr(res, "level") <- level
  ##
  res
}
