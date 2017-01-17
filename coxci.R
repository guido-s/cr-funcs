coxci <- function(x, sel = 1, level = 0.95, digits = 2) {
  HR <- exp(coef(x))[sel]
  ##
  ci <- exp(confint(x, level = level))
  lower <- ci[sel, 1]
  upper <- ci[sel, 2]
  ##
  res <- data.frame(HR = round(HR, digits = 2),
                    lower = round(lower, digits = 2),
                    upper = round(upper, digits = 2))
  ##
  res
}
