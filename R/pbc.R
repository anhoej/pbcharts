#' Process behaviour chart
#'
#' Draws run and control charts.
#'
#' See examples at \url{https://github.com/anhoej/pbcharts/}.
#'
#' @param x Numeric or date(time) vector of subgroup values to plot along the x
#'          axis. Or, if y is NULL, x values will be used for y coordinates.
#' @param num Numeric vector of measures or counts to
#'            plot on the y axis (numerator).
#' @param den Numeric vector of subgroup sizes (denominator).
#' @param facet Character (or factor) vector used for faceting.
#' @param data Data frame containing the variables used in the plot.
#' @param chart Character value indicating the chart type. Possible values are:
#'             'run' (default), 'i', and 'ms'.
#' @param freeze Integer indicating the index of the last subgroup in the
#'               baseline period (phase 1). Ignored if split is not NULL.
#' @param split Integer indicating the index of the last subgroup before
#'              splitting the graph.
#' @param exclude Integer vector of indices to exclude from calculations.
#' @param multiply Number to multiply y axis by, e.g. 100 to get percentages
#'                 rather than proportions.
#' @param ncol Number of columns in faceted plot.
#' @param ylim Numeric vector (y1, y2) setting the y axis limits. Useful e.g.
#'             for preventing negative negative control limits (c(0, NA) or
#'             proportions above 1 (100%) (c(0, 1).
#' @param yfixed Logical, if TRUE (default) makes a common scale for y
#'                    axes.
#' @param title,xlab,ylab Characters setting the main chart title and axis
#'                        labels. Use NULL to suppress.
#' @param partlabs Character vector of length two setting the labels for phase 1
#'                 and phase 2 periods.
#' @param plot Logical, if TRUE (default), plots an SPC chart.
#'
#' @returns Invisibly returns a list of class 'pbc'.
#' @export
#'
#' @examples
#' # Plot a run chart from 12 random normal values
#' pbc(rnorm(12))
#'
#' # Plot a control chart from 12 random normal values
#' pbc(rnorm(12), chart = 'i')
#'
pbc <- function(x,
                num      = NULL,
                den      = 1,
                facet    = NULL,
                data     = NULL,
                chart    = c('run', 'i', 'ms'),
                freeze   = NULL,
                split    = NULL,
                exclude  = NULL,
                multiply = 1,
                ncol     = NULL,
                ylim     = NULL,
                yfixed   = TRUE,
                title    = '', #NULL,
                xlab     = 'Subgroup',
                ylab     = 'Value',
                partlabs = c('phase 1', 'phase 2'),
                plot     = TRUE) {
  # Build title
  if (!is.null(title) && title == '') {
    title       <- deparse(substitute(num))

    if (title == 'NULL')
      title <- deparse(substitute(x))

    denominator <- deparse(substitute(den))

    if (denominator != 1)
      title <- paste(title, '/', denominator)

    if (multiply != 1)
      title <- paste(title, 'x', multiply)

    title <- paste(toupper(match.arg(chart)), 'Chart', 'of', title)
  }

  # Get data from data frame if data argument is provided, or else get data
  # from the parent environment.
  x     <- eval(substitute(x), data, parent.frame())
  num   <- eval(substitute(num), data, parent.frame())
  den   <- eval(substitute(den), data, parent.frame())
  facet <- eval(substitute(facet), data, parent.frame())

  # Get chart function
  chart     <- match.arg(chart)
  chart.fun <- get(paste0('pbc.', chart))

  # If num argument is missing, use x instead.
  if (is.null(num)) {
    num <- x
    x   <- seq_along(num)
  }

  # Get number of subgroups
  x.len <- length(unique(x))

  # Make sure that the num and den vectors are of the same length.
  if (length(den) == 1) {
    den <- rep(den, length(num))
  }

  # Make sure that numerators and denominators are balanced. If one is missing,
  # the other should be missing too.
  xna      <- !stats::complete.cases(num, den)
  num[xna] <- NA
  den[xna] <- NA

  # Ignore invalid freeze argument.
  if (!is.null(freeze) && (freeze < 2 || freeze > x.len - 2)) {
    freeze <- NULL
    message('Invalid freeze argument, ignoring.')
  }

  # Handle split argument
  if (!is.null(split) && (split < 2 || split > x.len - 2)) {
    split <- FALSE
    message('Invalid split argument, ignoring')
  } else if (!is.null(split)) {
    freeze <- split
    split <- TRUE
  } else {
    split <- FALSE
  }

  # Indices of baseline period (<= freeze/split)
  if (is.null(freeze)) {
    base <- seq_len(x.len)
  } else {
    base <- seq_len(freeze)
  }

  # Ignore invalid exclude argument
  if (!is.null(exclude) &&
      (any(exclude > length(unique(x))) || any(exclude < 1))) {
    exclude <- NULL
    message('Invalid exclude argument, ignoring.')
  }

  # Make dummy facet if facet is null
  if (is.null(facet)) {
    facet <- rep('1', length(x))
  }

  # Make y values to plot.
  y <- num / den

  # Make data frame.
  d <- data.frame(x, num, den, y, facet)

  # Do calculations for each facet.
  d <- split(d, facet)
  d <- lapply(d, chart.fun, base, split, exclude)
  d <- lapply(d, function(x) {
    x$useful              <- TRUE
    x$useful[exclude]     <- FALSE
    x$useful[x$y == x$cl | is.na(x$y)] <- FALSE
    x$phase               <- '1'
    x$phase[-base]        <- '2'
    x
  })

  d <- do.call(rbind, args = c(d, make.row.names = FALSE))

  # Sigma signal
  d$sigma.signal <- (d$y < d$lcl | d$y > d$ucl)

  # Censor control limits to ylim argument.
  if (!is.null(ylim)) {
    d$lcl <- pmax(d$lcl, ylim[1], na.rm = TRUE)
    d$ucl <- pmin(d$ucl, ylim[2], na.rm = TRUE)
  }

  # Multiply y coordinates if needed.
  d$y   <- d$y * multiply
  d$cl  <- d$cl * multiply
  d$lcl <- d$lcl * multiply
  d$ucl <- d$ucl * multiply

  d <- d[c('facet', 'phase', 'x', 'num', 'den', 'y',
           'lcl', 'cl', 'ucl', 'useful', 'runs.signal', 'sigma.signal')]

  d <- list(title    = title,
            xlab     = xlab,
            ylab     = ylab,
            base     = base,
            ncol     = ncol,
            yfixed   = yfixed,
            freeze   = freeze,
            split    = split,
            exclude  = exclude,
            partlabs = partlabs,
            chart    = chart,
            data     = d)

  # Draw plot.
  if (plot) {
    plot.pbc(d)
  }

  # Make data frame an 'spc' object and return invisibly.
  class(d) <- c('pbc', class(d))
  invisible(d)
}
