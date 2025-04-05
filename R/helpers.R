# Limits functions #############################################################

# Run chart
pbc.run <- function(x, base, split, exclude) {
  x$cl  <- stats::median(x$y[x$base], na.rm = TRUE)

  # # Ignore excluded values from calculations
  # y <- x$y
  #
  # if (!is.null(exclude)) {
  #   y[exclude] <- NA
  # }
  #
  # # Centre line and runs analysis
  # x$cl                 <- stats::median(y[base], na.rm = TRUE)
  # x$runs.signal        <- runs.analysis(y[base], x$cl[1])
  # x$runs.signal[-base] <- runs.analysis(y[-base], x$cl[-base][1])
  #
  # if (split) {
  #   x$cl[-base] <- stats::median(y[-base], na.rm = TRUE)
  # }

  # Control limits
  x$lcl <- NA_real_
  x$ucl <- NA_real_

  x
}

# I prime chart
pbc.i <- function(x, base, split, exclude) {
  x$cl <- stats::weighted.mean(x$y[x$base],
                               x$den[x$base],
                               na.rm = TRUE)
  s     <- c(NA, moving.s(x$y, x$den))
  sbar  <- sbar(s[x$base])
  stdev <- sbar * sqrt(1 / x$den)

  # # Ignore excluded values from calculations
  # y   <- x$y
  # den <- x$den
  #
  # if (!is.null(exclude)) {
  #   y[exclude]   <- NA
  #   den[exclude] <- NA
  # }
  #
  # # Centre line, runs analysis, and standard deviation
  # x$cl                 <- stats::weighted.mean(y[base],
  #                                              den[base],
  #                                              na.rm = TRUE)
  # x$runs.signal        <- runs.analysis(y[base], x$cl[1])
  # x$runs.signal[-base] <- runs.analysis(y[-base], x$cl[-base][1])
  # s                    <- c(NA, moving.s(y, den))
  # sbar                 <- sbar(s[base])
  # stdev                <- sbar * sqrt(1 / x$den)
  #
  # if (split) {
  #   x$cl[-base]  <- stats::weighted.mean(y[-base],
  #                                        den[-base],
  #                                        na.rm = TRUE)
  #   sbar         <- sbar(s[-base])
  #   stdev[-base] <- sbar * sqrt(1 / x$den[-base])
  # }

  # Control limits
  x$lcl <- x$cl - 3 * stdev
  x$ucl <- x$cl + 3 * stdev

  x
}

# Moving S prime chart
pbc.ms <- function(x, base, split, exclude) {
  s    <- c(NA, moving.s(x$y, x$den))
  x$y  <- s
  x$cl <- mean(s[x$base], na.rm = TRUE)

  # # Ignore excluded values from calculations
  # y   <- x$y
  # den <- x$den
  #
  # if (!is.null(exclude)) {
  #   y[exclude]   <- NA
  #   den[exclude] <- NA
  # }
  #
  # # Standard deviation - add NA to make s same length as y.
  # s    <- c(NA, moving.s(y, den))
  #
  # # Y values and centre line
  # x$y         <- s
  # x$cl        <- mean(s[base], na.rm = TRUE)
  #
  # if (split) {
  #   x$cl[-base] <- mean(s[-base], na.rm = TRUE)
  # }

  # Control limits
  x$ucl <- 3.2665 * x$cl
  x$lcl <- NA

  # # Don't do runs analysis
  # x$runs.signal <- FALSE

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
#
runs.analysis <- function(x) {
  runs     <- sign(x$y[x$include] - x$cl[x$include])
  runs     <- runs[runs != 0 & !is.na(runs)]
  n.useful <- length(runs)
  n.obs    <- nrow(x)

  if (n.useful) {
    run.lengths      <- rle(runs)$lengths
    n.runs           <- length(run.lengths)
    longest.run      <- max(run.lengths)
    longest.run.max  <- round(log2(n.useful)) + 3  # Schilling 2012
    n.crossings      <- max(n.runs - 1, 0)
    n.crossings.min  <- stats::qbinom(0.05,        # Chen 2010
                                      max(n.useful - 1, 0), 0.5)
    runs.signal      <- longest.run > longest.run.max ||
      n.crossings < n.crossings.min
  } else {
    longest.run     <- NA
    longest.run.max <- NA
    n.crossings     <- NA
    n.crossings.min <- NA
    runs.signal     <- FALSE
  }

  x$n.obs           <- n.obs
  x$n.useful        <- n.useful
  x$longest.run     <- longest.run
  x$longest.run.max <- longest.run.max
  x$n.crossings     <- n.crossings
  x$n.crossings.min <- n.crossings.min
  x$runs.signal     <- runs.signal

  x
}

# runs.analysis <- function(x, cl) {
#   if (!length(x))
#     return(FALSE)
#
#   n.obs <- length(x)
#
#   # Trichotomise data according to position relative to CL:
#   # -1 = below, 0 = on, 1 = above.
#   runs <- sign(x - cl)
#
#   # Remove NAs and data points on the centre line.
#   runs <- runs[runs != 0 & !is.na(runs)]
#
#   # Find run lengths.
#   run.lengths <- rle(runs)$lengths
#
#   # Find number of useful observations (data points not on centre line).
#   n.useful <- sum(run.lengths)
#
#   # Find longest run above or below centre line.
#   longest.run <- max(run.lengths)
#
#   # Find number of crossings.
#   n.crossings <- length(run.lengths) - 1
#
#   # Find upper limit for longest run.
#   longest.run.max <- round(log2(n.useful)) + 3
#
#   # Find lower limit for number of crossing.
#   n.crossings.min <- stats::qbinom(0.05, n.useful - 1, 0.5)
#
#   # Return result.
#   longest.run > longest.run.max | n.crossings < n.crossings.min
# }

# Moving S function ############################################################
#
# Calculates normalised moving standard deviations.
#
# Returns a vector of length one less than the input variables.
#
# y:   Numerator values.
# den: Denominator values.
#
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

# Make parts function ##########################################################
make.parts <- function(x, n) {
  x <- unique(c(0, x))
  x <- sort(x)
  rep(seq_along(x), diff(c(x, n)))
}
