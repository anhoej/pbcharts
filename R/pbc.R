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
                title    = '',
                xlab     = 'Subgroup',
                ylab     = 'Value',
                plot     = TRUE) {

  # Build title
  if (!is.null(title) && title == '') {
    title <- deparse(substitute(num))

    if (title == 'NULL')
      title <- deparse(substitute(x))

    denominator <- deparse(substitute(den))

    if (denominator != 1)
      title <- paste(title, '/', denominator)

    if (multiply != 1)
      title <- paste(title, 'x', multiply)

    title <- paste(toupper(match.arg(chart)), 'chart of', title)
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

  # Make dummy facet if facet is null
  if (is.null(facet)) {
    facet <- 1 #rep(1, length(x))
  }

  # Make sure that the num and den vectors are of the same length.
  if (length(den) == 1) {
    den <- rep(den, length(num))
  }

  # Make sure that numerators and denominators are balanced. If one is missing,
  # the other should be missing too.
  xna      <- !stats::complete.cases(num, den)
  num[xna] <- NA
  den[xna] <- NA

  # # Indices of baseline period (<= freeze/split)
  # if (is.null(freeze)) {
  #   base <- seq_len(x.len)
  # } else {
  #   base <- seq_len(freeze)
  # }

  # Ignore invalid exclude argument
  if (any(exclude > length(unique(x))) || any(exclude < 1)) {
    exclude <- NULL
    message('Invalid exclude argument, ignoring.')
  }

  # Ignore invalid freeze argument.
  if (!is.null(freeze) && (freeze < 2 || freeze > x.len - 2)) {
    freeze <- NULL
    message('Invalid freeze argument, ignoring.')
  }

  # Ignore invalid split argument
  if ((any(split < 2) || any(split > x.len - 2))) {
    split <- NULL
    message('Invalid split argument, ignoring')
  }

  # Ignore freeze when split is set
  if (!is.null(split) && !is.null(freeze)) {
    freeze <- NULL
    message('Ignoring freeze when split is set')
  }

  # Make y values to plot.
  y <- num / den

  # Make data frame.
  d <- data.frame(x, num, den, y, facet)

  # Split facets by split argument.
  d <- split(d, ~ facet)
  d <- lapply(d, function(x) {
    x$part <- make.parts(split, nrow(x))
    x$xx   <- seq_along(x$part)
    x
  })
  d <- do.call(rbind, args = c(d, make.row.names = FALSE))

  # Helper variables
  d$freeze  <- d$xx <= min(freeze, Inf)
  d$include <- !d$xx %in% exclude
  d$base    <- d$freeze & d$include

  # Control limits and runs analysis for each facet and part.
  d <- split(d, ~ facet + part)
  d <- lapply(d, chart.fun)
  d <- lapply(d, runs.analysis)
  d <- do.call(rbind, args = c(d, make.row.names = FALSE))

  # Sigma signal
  d$sigma.signal <- d$y < d$lcl | d$y > d$ucl

  # Censor control limits to ylim argument.
  if (!is.null(ylim) && chart != 'run') {
    d$lcl <- pmax(d$lcl, ylim[1], na.rm = TRUE)
    d$ucl <- pmin(d$ucl, ylim[2], na.rm = TRUE)
  }

  # Multiply y coordinates if needed.
  d$y   <- d$y * multiply
  d$cl  <- d$cl * multiply
  d$lcl <- d$lcl * multiply
  d$ucl <- d$ucl * multiply

  d <- d[order(d$facet, d$part, d$x),]

  d <- d[c('facet', 'part', 'x', 'num', 'den', 'y',
           'lcl', 'cl', 'ucl', 'runs.signal', 'sigma.signal',
           'freeze', 'include', 'base', 'n.obs', 'n.useful')]

  d <- list(title    = title,
            xlab     = xlab,
            ylab     = ylab,
            # base     = base,
            ncol     = ncol,
            yfixed   = yfixed,
            freeze   = freeze,
            split    = split,
            exclude  = exclude,
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
