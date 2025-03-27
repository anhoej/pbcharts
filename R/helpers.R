# Limits functions #############################################################

# Run chart
pbc.run <- function(x, base, split, exclude) {
  # Ignore excluded values from calculations
  y <- x$y

  if (!is.null(exclude)) {
    y[exclude] <- NA
  }

  # Centre line
  x$cl  <- stats::median(y[base], na.rm = TRUE)

  if (split) {
    x$cl[-base]  <- stats::median(y[-base], na.rm = TRUE)
  }

  # Runs signal
  x$runs.signal        <- runs.analysis(y[base], x$cl[1])
  x$runs.signal[-base] <- runs.analysis(y[-base], x$cl[-base][1])

  # Control limits
  x$lcl <- NA_real_
  x$ucl <- NA_real_

  # Phase: 1 = before freeze/split, 2 = after freeze/split
  x$phase        <- '1'
  x$phase[-base] <- '2'

  x
}

# I prime chart
pbc.i <- function(x, base, split, exclude) {
  # Ignore excluded values from calculations
  y   <- x$y
  den <- x$den

  if (!is.null(exclude)) {
    y[exclude]   <- NA
    den[exclude] <- NA
  }

  # Centre line
  x$cl <- stats::weighted.mean(y[base], den[base], na.rm = TRUE)

  # Standard deviation - add NA to make s same length as y.
  s     <- c(NA, moving.s(y, den))
  sbar  <- sbar(s[base])
  stdev <- sbar * sqrt(1 / x$den)

  # stdev and centre line after split
  if (split) {
    sbar         <- sbar(s[-base])
    stdev[-base] <- sbar * sqrt(1 / x$den[-base])
    x$cl[-base]  <- stats::weighted.mean(y[-base], den[-base], na.rm = TRUE)
  }

  # Runs analysis
  x$runs.signal        <- runs.analysis(y[base], x$cl[1])
  x$runs.signal[-base] <- runs.analysis(y[-base], x$cl[-base][1])

  # Control limits
  x$lcl <- x$cl - 3 * stdev
  x$ucl <- x$cl + 3 * stdev

  # Phase: 1 = before freeze/split, 2 = after freeze/split
  x$phase        <- '1'
  x$phase[-base] <- '2'

  x
}

# Moving S prime chart
pbc.ms <- function(x, base, split, exclude) {
  # Ignore excluded values from calculations
  y   <- x$y
  den <- x$den

  if (!is.null(exclude)) {
    y[exclude]   <- NA
    den[exclude] <- NA
  }

  # Standard deviation - add NA to make s same length as y.
  s    <- c(NA, moving.s(y, den))

  # Y values and centre line
  x$y         <- s
  x$cl        <- mean(s[base], na.rm = TRUE)

  if (split) {
    x$cl[-base] <- mean(s[-base], na.rm = TRUE)
  }

  # Control limits
  x$ucl <- 3.2665 * x$cl
  x$lcl <- NA

  # Don't do runs analysis
  x$runs.signal <- FALSE

  # Phase: 1 = before freeze/split, 2 = after freeze/split
  x$phase        <- '1'
  x$phase[-base] <- '2'

  x
}


# Runs analysis function #######################################################
#
#  Tests data for non-random variation in the form of unusually long runs or
#  unusually few crossings.
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

# Moving S function ############################################################
#
# Calculates normalised moving standard deviations.
#
# Returns a vector of length one less than the input variables.
#
# y:   Numerator values.
# den: Denominator values.

moving.s <- function(y, den) {
  l     <- length(y)
  d1    <- abs(diff(y, na.rm = TRUE))
  d2    <- sqrt((1 / den[1:(l - 1)]) + (1 / den[2:l]))
  s     <- sqrt(pi / 2) * d1 / d2
  s
}

# Sbar function ################################################################
#
# Calculates screened Sbar.
#
# Returns a number, the screened average moving S.

sbar <- function(s) {
  sbar       <- mean(s, na.rm = TRUE)
  uls        <- sbar* 3.2665
  s[s > uls] <- NA
  mean(s, na.rm = TRUE)
}
