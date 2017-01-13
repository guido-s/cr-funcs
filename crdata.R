crdata <- function(time, event, initial.states, data, cens.name) {
  ##
  ## Create competing risks dataset
  ## (input for mvna() and etm() of respective R packages)
  ##
  nulldata <- is.null(data)
  ##
  if (nulldata)
    data <- sys.frame(sys.parent())
  ##
  mf <- match.call()
  ##
  ## Catch time, event, and initial.states
  ##
  time <- eval(mf[[match("time", names(mf))]],
               data, enclos = sys.frame(sys.parent()))
  event <- eval(mf[[match("event", names(mf))]],
                data, enclos = sys.frame(sys.parent()))
  initial.states <- eval(mf[[match("initial.states", names(mf))]],
                         data, enclos = sys.frame(sys.parent()))
  ##
  len <- length(time)
  ##
  if (length(event) != len)
    stop("Arguments 'time' and 'event' must be of same length.")
  if (length(initial.states) != 1 && length(initial.states) != len)
    stop("Argument 'initial.states' must be a character string or of same length as argument 'time.")
  ##
  if (length(initial.states) == 1)
    initial.states <- rep(initial.states, len)
  ##
  res <- rbind(data.frame(id = 0, from = initial.states[1], to = cens.name,
                         time = min(1e-6, 0.99 * min(time, na.rm = TRUE))),
               data.frame(id = seq_len(len),
                          from = initial.states, to = event,
                          time = time))
  ##
  res
}
