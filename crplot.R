crplot <- function(x, y,
                   tr.choice1, tr.choice2,
                   main1 = deparse(substitute(x)),
                   main2 = deparse(substitute(y)),
                   plottype = "subgroup",
                   text.legend1 = "", text.legend2,
                   lty = c(1, 1), lwd = 1.5, col = c("black", "darkgrey"),
                   pty = "s", xlab = "", ylab = "",
                   x.add, y.add,
                   bland.altman = TRUE,
                   ...) {
  ##
  if (length(lwd) == 1)
    lwd <- rep_len(lwd, length(lty))
  ##
  if (missing(text.legend2))
    text.legend2 <- text.legend1
  ##
  plottype <- setchar(plottype, c("subgroup", "transition", "goodness.of.fit"))
  ##
  if (!missing(y) & plottype == "subgroup") {
    ##
    if (missing(ylab))
      if (inherits(x, "etm"))
        ylab <- "Probability"
      else if (inherits(x, "mvna"))
        ylab <- "Cumulative Hazard"
    ##
    oldpar <- par(pty = pty)
    on.exit(par(oldpar))
    ##
    split.screen(figs = c(1, 2))
    ##
    screen(1)
    ##
    oldpar1 <- par(mar = c(5, 0, 4, 0) + 0.1,
                   oma = c(0, 4.5, 0, 1.5))
    ##
    plot(x,
         tr.choice = tr.choice1,
         lty = lty[1], lwd = lwd[1], col = col[1],
         main = main1,
         legend = FALSE,
         xlab = xlab, ylab = "",
         ...)
    ##
    mtext(ylab, side = 2, line = 3)
    ##
    lines(y, tr.choice = tr.choice1,
          lty = lty[2], lwd = lwd[2], col = col[2])
    ##
    if (!missing(x.add))
      lines(x.add$time, x.add$pstate2,
            lty = lty[3], lwd = lwd[3], col = col[3],
            type = "s")
    if (!missing(y.add))
      lines(y.add$time, y.add$pstate2,
            lty = lty[4], lwd = lwd[4], col = col[4],
            type = "s")
    ##
    legend(x = "topleft",
           lty = lty, lwd = lwd, col = col,
           legend = text.legend1,
           bty = "n")
    ##
    screen(2)
    ##
    oldpar2 <- par(mar = c(5, 0, 4, 0) + 0.1,
                   oma = c(0, 4.5, 0, 1.5))
    ##
    plot(x,
         tr.choice = tr.choice2,
         lty = lty[1], lwd = lwd[1], col = col[1],
         main = main2,
         legend = FALSE, axes = FALSE,
         xlab = xlab, ylab = "",
         ...)
    ##
    axis(1)
    box()
    lines(y, tr.choice = tr.choice2,
          lty = lty[2], lwd = lwd[2], col = col[2])
    ##
    if (!missing(x.add))
      lines(x.add$time, x.add$pstate3,
            lty = lty[3], lwd = lwd[3], col = col[3],
            type = "s")
    if (!missing(y.add))
      lines(y.add$time, y.add$pstate3,
            lty = lty[4], lwd = lwd[4], col = col[4],
            type = "s")
    ##
    legend(x = "topleft",
           lty = lty, lwd = lwd, col = col,
           legend = text.legend2,
           bty = "n")
    ##
    close.screen(all.screens = TRUE)
    par(oldpar1)
  }
  ##
  else if (!missing(y) & plottype == "transition") {
    ##
    if (missing(ylab))
      if (inherits(x, "etm"))
        ylab <- "Probability"
      else if (inherits(x, "mvna"))
        ylab <- "Cumulative Hazard"
    ##
    oldpar <- par(pty = pty)
    on.exit(oldpar)
    ##
    split.screen(figs = c(1, 2))
    ##
    screen(1)
    ##
    oldpar1 <- par(mar = c(5, 0, 4, 0) + 0.1,
                   oma = c(0, 4.5, 0, 1.5))
    ##
    plot(x,
         tr.choice = tr.choice1,
         legend = FALSE,
         main = main1,
         lty = lty[1:2], lwd = lwd[1:2], col = col[1:2],
         xlab = xlab, ylab = "",
         ...)
    ##
    mtext(ylab, side = 2, line = 3)
    ##
    if (!missing(x.add))
      lines(x.add$time, x.add$pstate2,
            lty = lty[3], lwd = lwd[3], col = col[3],
            type = "s")
    if (!missing(y.add))
      lines(x.add$time, x.add$pstate3,
            lty = lty[4], lwd = lwd[4], col = col[4],
            type = "s")
    ##
    legend(x = "topleft",
           lty = lty, lwd = lwd, col = col,
           legend = text.legend1,
           bty = "n")
    ##
    screen(2)
    ##
    oldpar2 <- par(mar = c(5, 0, 4, 0) + 0.1,
                   oma = c(0, 4.5, 0, 1.5))
    ##
    plot(y,
         tr.choice = tr.choice2,
         legend = FALSE,
         main = main2,
         lty = lty[1:2], lwd = lwd[1:2], col = col[1:2],
         axes = FALSE,
         xlab = xlab, ylab = "",
         ...)
    ##
    axis(1)
    box()
    ##
    if (!missing(x.add))
      lines(y.add$time, y.add$pstate2,
            lty = lty[3], lwd = lwd[3], col = col[3],
            type = "s")
    if (!missing(y.add))
      lines(y.add$time, y.add$pstate3,
            lty = lty[4], lwd = lwd[4], col = col[4],
            type = "s")
    ##
    legend(x = "topleft",
           lty = lty, lwd = lwd, col = col,
           legend = text.legend2,
           bty = "n")
    ##
    close.screen(all.screens = TRUE)
    par(oldpar1)
  }
  ##
  else if (!missing(y) & plottype == "goodness.of.fit") {
    ##
    if (missing(xlab))
      xlab <- "Average Cumulative Hazard"
    if (missing(ylab))
      ylab <- "Difference of Cumulative Hazards"
    ##
    time1 <- x[[tr.choice1]]$time
    time2 <- y[[tr.choice1]]$time
    times <- unique(sort(c(time1, time2)))
    ##
    ind1 <- findInterval(times, time1)
    ind2 <- findInterval(times, time2)
    ind1[ind1 == 0] <- NA
    ind2[ind2 == 0] <- NA
    ##
    NE1 <- x[[tr.choice1]]$na[ind1]
    NE2 <- y[[tr.choice1]]$na[ind2]
    ##
    oldpar <- par(pty = pty)
    on.exit(oldpar)
    ##
    split.screen(figs = c(1, 2))
    ##
    screen(1)
    ##
    oldpar1 <- par(mar = c(5, 0, 4, 0) + 0.1,
                   oma = c(0, 4.5, 0, 1.5))
    ##
    if (bland.altman) {
      NE1 <- NE1 * exp(x.add$coef)
      xpos <- (NE1 + NE2) / 2
      ypos = NE1 - NE2
    }
    ##
    plot(xpos, ypos, type = "n", main = main1,
         lty = lty[1], lwd = lwd[1], col = col[1],
         xlab = xlab, ylab = "",
         ...)
    ##
    if (bland.altman)
      abline(h = 0, col = "darkgray", lwd = lwd[1])
    else
      abline(a = 0, b = exp(x.add$coef), col = "darkgray",
             lwd = lwd[1])
    ##
    lines(xpos, ypos,
          type = "s",
          lty = lty[1], lwd = lwd[1], col = col[1])
    ##
    mtext(ylab, side = 2, line = 3)
    ##
    screen(2)
    ##
    time1 <- x[[tr.choice2]]$time
    time2 <- y[[tr.choice2]]$time
    times <- unique(sort(c(time1, time2)))
    ##
    ind1 <- findInterval(times, time1)
    ind2 <- findInterval(times, time2)
    ind1[ind1 == 0] <- NA
    ind2[ind2 == 0] <- NA
    ##
    NE1 <- x[[tr.choice2]]$na[ind1]
    NE2 <- y[[tr.choice2]]$na[ind2]
    ##
    oldpar2 <- par(mar = c(5, 0, 4, 0) + 0.1,
                   oma = c(0, 4.5, 0, 1.5))
    ##
    ##
    if (bland.altman) {
      NE1 <- NE1 * exp(y.add$coef)
      xpos <- (NE1 + NE2) / 2
      ypos = NE1 - NE2
    }
    ##
    plot(xpos, ypos, type = "n", main = main2,
         lty = lty[1], lwd = lwd[1], col = col[1],
         xlab = xlab, ylab = "",
         axes = FALSE, ...)
    ##
    if (bland.altman)
      abline(h = 0, col = "darkgray", lwd = lwd[1])
    else
      abline(a = 0, b = exp(y.add$coef), col = "darkgray",
             lwd = lwd[1])
    ##
    lines(xpos, ypos, type = "s",
          lty = lty[1], lwd = lwd[1], col = col[1])
    ##
    axis(1)
    box()
    ##
    close.screen(all.screens = TRUE)
    par(oldpar1)
  }
  else if (missing(y)) {
    ##
    if (missing(ylab))
      if (inherits(x, "etm"))
        ylab <- "Probability"
      else if (inherits(x, "mvna"))
        ylab <- "Cumulative Hazard"
    ##
    oldpar <- par(pty = pty)
    on.exit(par(oldpar))
    ##
    plot(x,
         tr.choice = tr.choice1,
         legend = FALSE,
         main = main1,
         lty = lty, lwd = lwd, col = col,
         ylab = ylab,
         ...)
    ##
    legend(x = "topleft",
           lty = lty, lwd = lwd, col = col,
           legend = text.legend1,
           bty = "n")
  }
  else
    warning("Plot method not defined.")
  ##
  invisible(NULL)
}
