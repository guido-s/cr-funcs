lrtest <- function(x, y, digits = 4) {
  if (!inherits(x, "coxph"))
    stop("Argument 'x' must be an object of class 'coxph'.")
  if (!missing(y) && !inherits(y, "coxph"))
    stop("Argument 'y' must be an object of class 'coxph'.")
  ##
  if (missing(y)) {
    deviance <- 2 * diff(x$loglik)
    df <- length(coef(x)) - 1
  }
  else {
    if (x$loglik[2] < y$loglik[2])
      stop("Cox model 'x' has smaller log likelihood than Cox model 'y'.")
    if (length(coef(x)) < length(coef(y)))
      stop("Cox model 'x' has fewer coefficients than Cox model 'y'.")
    ##
    deviance <- 2 * (x$loglik[2] - y$loglik[2])
    df <- length(coef(x)) - length(coef(y))
  }
  res <- data.frame(Chi2 = round(deviance, 2), df = df,
                    p = 1 - round(pchisq(deviance, df), digits = digits))
  res
}
