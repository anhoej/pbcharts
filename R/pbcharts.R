# Master SPC function ##########################################################
#
# Constructs an SPC chart.
#
#  Invisibly returns a data frame of class 'pbc'.
#
#  x:        Numeric or date(time) vector of subgroup values to plot along the x
#            axis. Or, if y is NULL, x values will be used for y coordinates.
#  num:      Numeric vector of measures or counts to
#            plot on the y axis (numerator).
#  den:      Numeric vector of subgroup sizes (denominator).
#  z:        Character (or factor) vector used for faceting.
#  data:     Data frame containing the variables used in the plot.
#  chart:    Character value indicating the chart type. Possible values are:
#            'run' (default), and 'i'.
#  multiply: Number to multiply y axis by, e.g. 100 to get percentages rather
#            than proportions.
#  ncol:
#  title:
#  xlab:
#  ylab:
#  plot:     Logical, if TRUE (default), plots an SPC chart.
#  print:    Logical, if TRUE, prints a data frame with coordinates.
#' @param x
#'
#' @param num
#' @param den
#' @param z
#' @param data
#' @param chart
#' @param multiply
#' @param ylim
#' @param ncol
#' @param freeze
#' @param title
#' @param xlab
#' @param ylab
#' @param partlabs
#' @param plot
#' @param print
#'
#' @export
pbc <- function(x,
                num      = NULL,
                den      = 1,
                z        = NULL,
                data     = NULL,
                chart    = c('run', 'i'),
                multiply = 1,
                ylim     = NULL,
                ncol     = NULL,
                freeze   = NULL,
                title    = NULL,
                xlab     = 'Subgroup',
                ylab     = 'Value',
                partlabs = c('phase 1', 'phase 2'),
                plot     = TRUE,
                print    = FALSE) {
  # Get data from data frame if data argument is provided, or else get data
  # from the parent environment.
  x   <- eval(substitute(x), data, parent.frame())
  num <- eval(substitute(num), data, parent.frame())
  den <- eval(substitute(den), data, parent.frame())
  z   <- eval(substitute(z), data, parent.frame())

  # Get chart function
  chart     <- match.arg(chart)
  chart.fun <- get(paste0('pbc.', chart))


  # If num argument is missing, use x instead.
  if (is.null(num)) {
    num <- x
    x   <- seq_along(num)
  }

  # Make sure that the num and den vectors are of the same length.
  if (length(den) == 1) {
    den <- rep(den, length(num))
  }

  # Make sure that numerators and denominators are balanced. If one is missing,
  # the other should be missing too.
  xna    <- !stats::complete.cases(num, den)
  num[xna] <- NA
  den[xna] <- NA

  # Make y values to plot.
  y <- num / den

  # Make dummy facet if z is null
  if (is.null(z)) {
    z <- rep('1', length(num))
  }

  # Make data frame.
  d <- data.frame(x, num, den, y, z)

  # Calculate sigma limits and perform runs analysis to each facet.
  d <- split(d, z)
  d <- lapply(d, chart.fun, freeze)
  d <- do.call(rbind, args = c(d, make.row.names = F))

  # Set y limits
  if (!is.null(ylim)) {
    d$lcl <- pmax(d$lcl, ylim[1], na.rm = TRUE)
    d$ucl <- pmin(d$ucl, ylim[2], na.rm = TRUE)
  }


  # Find useful data points (not on centre line).
  d$useful <- d$y != d$cl

  # Multiply y coordinates if needed.
  d$y   <- d$y * multiply
  d$cl  <- d$cl * multiply
  d$lcl <- d$lcl * multiply
  d$ucl <- d$ucl * multiply

  # Make plot.
  if (plot) {
    plot.pbc(d, title, ncol, freeze, xlab, ylab, partlabs)
  }

  # Print data frame.
  if (print) {
    print(d)
  }

  # Make data frame an 'spc' object and return invisibly.
  class(d) <- c('pbc', class(d))
  invisible(d)
}

# Plot function ################################################################
plot.pbc <- function(x,
                     title    = NULL,
                     ncol     = NULL,
                     freeze   = NULL,
                     xlab     = 'Subgroup',
                     ylab     = 'Value',
                     partlabs = c('phase 1', 'phase 2')) {
  col1 <- 'steelblue'
  col2 <- 'grey30'
  col3 <- 'tomato'
  col4 <- 'gray'

  if (is.null(freeze)) {
    base <- seq_along(x$x)
  } else {
    base <- seq_len(freeze)
  }

  # Make room for data and control limits on the y axes.
  ylim <- range(x$y,
                x$lcl,
                x$ucl,
                na.rm = TRUE)

  # Find x axis format.
  if (inherits(x$x, 'Date')) {
    x_class <- graphics::axis.Date
  } else if (inherits(x$x, 'POSIXct')) {
    x_class <- graphics::axis.POSIXct
  } else {
    x_class <- graphics::axis
  }

  # Calculate plots layout.
  n_facets <- length(unique(x$z))
  n_cols   <- ifelse(is.null(ncol),
                     ceiling(sqrt(n_facets)),
                     ncol)
  n_rows   <- ceiling(n_facets / n_cols)

  # Prepare plot canvas.
  op <- graphics::par(mfrow = c(n_rows, n_cols),
                      mar   = c(3, 2, ifelse(n_facets == 1, 0, 2), 3),
                      oma   = c(2, 2.6, 3.5, 0))
  axis_par <- list(las       = 1,
                   lwd       = 0,
                   lwd.ticks = 1,
                   tcl       = -0.3,
                   col       = col2)

  # Populate facets.
  d <- split(x, x$z)
  for(i in d) {
    dotcol                 <- ifelse(i$useful, col1, col4)
    dotcol[i$sigma.signal] <- col3
    clcol                  <- ifelse(i$runs.signal[1], col3, col2)
    cltyp                  <- ifelse(i$runs.signal[1], 'dashed', 'solid')
    clcol2                 <- ifelse(i$runs.signal[freeze + 1],
                                     col3,
                                     col2)
    cltyp2                 <- ifelse(i$runs.signal[freeze + 1],
                                     'dashed',
                                     'solid')

    # Prepare plot.
    plot(i$x, i$y,
         type = 'n',
         axes = F,
         ylim = ylim,
         ylab = '',
         xlab = '')

    # Add lines and points to plot.
    graphics::lines(i$x[base], i$cl[base],
                    col = clcol,
                    lty = cltyp)
    graphics::lines(i$x[-base], i$cl[-base],
                    col = clcol2,
                    lty = cltyp2)
    graphics::lines(i$x[base], i$lcl[base],
                    col = col2)
    graphics::lines(i$x[base], i$ucl[base],
                    col = col2)
    graphics::lines(i$x[-base], i$lcl[-base],
                    col = col2)
    graphics::lines(i$x[-base], i$ucl[-base],
                    col = col2)
    graphics::lines(i$x, i$y,
                    col = col1,
                    lwd = 2.5)
    graphics::points(i$x, i$y,
                     cex = 0.8,
                     col = dotcol,
                     pch = 19)

    if (!is.null(freeze)) {
      # abline(v = mean(c(as.numeric(x$x[freeze]),
      #                   as.numeric(x$x[freeze + 1]))),
      #        lty = 'dotted')
      graphics::mtext(partlabs[1],
                      at   = mean(c(as.numeric(x$x[1]),
                                    as.numeric(x$x[freeze]))),
                      cex  = 0.7,
                      line = -0.9)
      graphics::mtext(partlabs[2],
                      at   = mean(c(as.numeric(x$x[freeze] + 1),
                                    as.numeric(utils::tail(x$x, 1)))),
                      cex  = 0.7,
                      line = -0.9)
    }

    # Add axes, box, and centre line label.
    graphics::box(bty = 'l', col = col2)
    do.call(x_class, c(1, axis_par))          # x axis
    do.call(graphics::axis, c(2, axis_par))   # y axis
    graphics::mtext(formatC(i$cl[1]),                   # cl label
          side = 4,
          at   = i$cl[1],
          adj  = 0,
          las  = 1,
          cex  = 0.7)

    # Add facet labels
    if (n_facets > 1)
      graphics::title(main = i$z[1], adj = 0, font.main = 1, line = 1.2)
  }

  # Main title and axis labels.
  graphics::mtext(xlab,
                  side  = 1,
                  line  = 0.5,
                  outer = TRUE)
  graphics::mtext(ylab,
                  side  = 2,
                  line  = 1,
                  outer = TRUE)
  graphics::mtext(title,
                  side  = 3,
                  line  = 1.3,
                  outer = TRUE,
                  font  = 1,
                  cex   = 1.2,
                  adj   = 0)

  # Reset plot canvas.
  graphics::par(op)
}

# Summary function #############################################################
summary.pbc <- function(x) {
  d <- split(x, x$z)
  d <- lapply(d, function(x) {
    data.frame(z = x$z[1],
               n = nrow(x),
               n.useful = sum(x$useful),
               avg_lcl = mean(x$lcl, na.rm = TRUE),
               cl = mean(x$cl, na.rm = TRUE),
               avg_ucl = mean(x$ucl, na.rm = TRUE),
               sigma.signal = sum(x$sigma.signal, na.rm = TRUE),
               runs.signal = as.integer(any(x$runs.signal)))
  })
  do.call(rbind, c(d, make.row.names = F))
}


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
