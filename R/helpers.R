# Limits functions #############################################################
#
#  Calculate centre line and control limits according to chart type.
#
#  Return data frames.
#
#  x:  Data frame from pbc().
#
#
# Run chart
pbc.run <- function(x, screenms) {
  # Centre line
  if (all(is.na(x$cl))) {
    x$cl  <- stats::median(x$y[x$base], na.rm = TRUE)
  }

  # Control limits
  x$lcl <- NA_real_
  x$ucl <- NA_real_

  x
}

# I prime chart
pbc.i <- function(x, screenms) {
  # Centre line
  if (all(is.na(x$cl))) {
    x$cl <- stats::weighted.mean(x$y[x$base],
                                 x$den[x$base],
                                 na.rm = TRUE)
  }

  # Standard deviation
  if (all(is.na(x$sd))) {
    s          <- c(NA, moving.s(x$y[x$base], x$den[x$base]))
    sbar       <- mean(s, na.rm = TRUE)

    if (screenms) {
      # Remove values above upper control limit
      uls        <- sbar * 3.2665
      s[s > uls] <- NA
      sbar       <- mean(s, na.rm = TRUE)
    }

    stdev <- sbar * sqrt(1 / x$den)
  } else {
    stdev <- x$sd
  }

  # Control limits
  x$lcl <- x$cl - 3 * stdev
  x$ucl <- x$cl + 3 * stdev

  x
}

# Moving S chart
pbc.ms <- function(x, screenms) {
  # Centre line
  s    <- c(NA, moving.s(x$y, x$den))
  x$y  <- s
  x$cl <- mean(s[x$base], na.rm = TRUE)

  # Control limits
  x$ucl <- 3.2665 * x$cl
  x$lcl <- NA

  x
}

# Runs analysis function #######################################################
#
#  Tests data for non-random variation in the form of unusually long runs or
#  unusually few crossings. Called from the pbc() function.
#
#  Returns a data frame
#
#  x:  Data frame from pbc().
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

# Make parts function ##########################################################
#
# Creates helper variable to split charts.
#
# x: Integer indicating the the last subgroup before splitting the graph.
# n: Integer indicating the number of subgroups in chart.
#
make.parts <- function(x, n) {
  x <- unique(c(0, x))
  x <- sort(x)
  rep(seq_along(x), diff(c(x, n)))
}

# Make labels function #########################################################
#
# Create nice labels for centre and control lines.
# x:    Value to convert to string.
# ypct: Logical, show label as percentage.
#
format_labs <- function(x, ypct) {
  if (ypct) {
    x <- formatC(x * 100, digits = 3, format = 'fg')
    x <- paste0(x, '%')
  } else {
    x <- formatC(x, digits = 3, format = 'fg')
  }

  x
}
