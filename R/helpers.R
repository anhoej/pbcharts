# Limits functions #############################################################

# Run chart
pbc.run <- function(x, freeze) {
  if (is.null(freeze)) {
    base <- seq_along(x$x)
  } else {
    base <- seq_len(freeze)
  }

  # Centre line
  x$cl  <- stats::median(x$y[base], na.rm = TRUE)

  # Runs signal
  x$runs.signal <- runs.analysis(x$y[base], x$cl[1])
  x$runs.signal[-base] <- runs.analysis(x$y[-base], x$cl[1])

  # Control limits
  x$lcl <- NA_real_
  x$ucl <- NA_real_

  x
}

# I prime chart
pbc.i <- function(x, freeze) {
  # Get indices of baseline period (<= freeze)
  if (is.null(freeze)) {
    base <- seq_along(x$x)
  } else {
    base <- seq_len(freeze)
  }

  # Centre line
  x$cl <- stats::weighted.mean(x$y[base], x$den[base], na.rm = TRUE)

  # Runs analysis
  x$runs.signal        <- runs.analysis(x$y[base], x$cl[1])
  x$runs.signal[-base] <- runs.analysis(x$y[-base], x$cl[1])

  # Standard deviation
  l     <- length(base)
  d1    <- abs(diff(x$y[base], na.rm = TRUE))
  d2    <- sqrt((1 / x$den[1:(l - 1)]) + (1 / x$den[2:l]))
  s     <- sqrt(pi / 2) * d1 / d2
  as    <- mean(s, na.rm = TRUE)

  # Remove s values above upper limit before calculating stdev
  uls   <- as * 3.2665
  s     <- s[s < uls]
  as    <- mean(s, na.rm = TRUE)
  stdev <- as * sqrt(1 / x$den)

  # Control limits
  x$lcl <- x$cl - 3 * stdev
  x$ucl <- x$cl + 3 * stdev

  # Sigma signal
  x$sigma.signal                        <- (x$y < x$lcl | x$y > x$ucl)
  x$sigma.signal[is.na(x$sigma.signal)] <- FALSE

  x
}


# Runs analysis function #######################################################
#
#  Tests data for non-random variation in the form of unusually long runs or
#  unusually few crossings. Called from the pbc() function.
#
#  Returns a logical, TRUE if non-random variation is found.
#
#  x:  Numeric vector.
#  cl: Single number, target value.

runs.analysis <- function(x, cl) {
  if (!length(x))
    return(FALSE)

  # Trichotomise data according to position relative to CL:
  # -1 = below, 0 = on, 1 = above.
  runs <- sign(x - cl)

  # Remove NAs and data points on the centre line.
  runs <- runs[runs != 0 & !is.na(runs)]

  # Find run lengths.
  run.lengths <- rle(runs)$lengths

  # Find number of useful observations (data points not on centre line).
  n.useful <- sum(run.lengths)

  # Find longest run above or below centre line.
  longest.run <- max(run.lengths)

  # Find number of crossings.
  n.crossings <- length(run.lengths) - 1

  # Find upper limit for longest run.
  longest.run.max <- round(log2(n.useful)) + 3

  # Find lower limit for number of crossing.
  n.crossings.min <- stats::qbinom(0.05, n.useful - 1, 0.5)

  # Return result.
  longest.run > longest.run.max | n.crossings < n.crossings.min
}
