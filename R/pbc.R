#' Process behaviour chart
#'
#' Draws run and control charts.
#'
#' See examples at \url{https://github.com/anhoej/pbcharts/}.
#'
#' @param x Numeric or date(time) vector of subgroup values to plot along the x
#'          axis. Or, if num is NULL, x values will be used for y coordinates.
#' @param num Numeric vector of measures or counts to
#'            plot on the y axis (numerator).
#' @param den Numeric vector of subgroup sizes (denominator).
#' @param facet List of variable(s) used for faceting.
#' @param data Data frame containing the variables used in the plot.
#' @param chart Character value indicating the chart type. Possible values are:
#'             'run' (default), 'i', and 'ms'.
#' @param freeze Integer indicating the index of the last subgroup in the
#'               baseline period (phase 1). Ignored if split is not NULL.
#' @param split Integer indicating the index of the last subgroup before
#'              splitting the graph.
#' @param exclude Integer vector of indices to exclude from calculations.
#' @param target Number indicating the target value to be drawn as a horisontal
#'               line.
#' @param screenms Logical, screen moving standard deviations before calculating
#'                 control limits by removing values above upper control limit
#'                 (3.2665).
#' @param multiply Number to multiply y axis by, e.g. 100 to get percentages
#'                 rather than proportions.
#' @param ncol Number of columns in faceted plot.
#' @param ylim Numeric vector (y1, y2) setting the y axis limits. Useful
#'             for preventing negative negative control limits (ylim = c(0, NA))
#'             or proportions above 1 (ylim = c(0, 1)).
#' @param yfixed Logical, if TRUE (default) makes a common scale for y
#'               axes in faceted plots.
#' @param ypct Logical, if TRUE displays the y axis labels as percentages and
#'             censors the y axis at 0% and 100%.
#' @param cl Numeric, single value indicating the centre line (if known).
#' @param sd Numeric, single value indicating the standard deviation (if known).
#' @param title,xlab,ylab Characters setting the main chart title and axis
#'                        labels. Use NULL to suppress.
#' @param plot Logical, if TRUE (default), plots an SPC chart.
#'
#' @returns Invisibly returns a list of class 'pbc'.
#' @export
#'
#' @examples
#' # Plot a run chart from 12 random normal values:
#' pbc(rnorm(12))
#'
#' # Plot a control chart:
#' pbc(rnorm(12), chart = 'i')
#'
#' # Plot a control chart of bacteremia mortality faceted by hospital:
#' pbc(month, deaths, cases,
#'     facet = hospital,
#'     data  = bacteremia_mortality,
#'     chart = 'i')
#'
#' # Assign output to variable:
#' p <- pbc(month, deaths, cases,
#'          facet = hospital,
#'          data  = bacteremia_mortality,
#'          chart = 'i',
#'          plot  = FALSE)
#'
#' # Plot pbc object:
#' plot(p)
#'
#' # Print summary of pbc object:
#' summary(p)
#'
#' # Print pbc object:
#' print(p)
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
                target   = NULL,
                screenms = FALSE,
                multiply = 1,
                ncol     = NULL,
                ylim     = NULL,
                yfixed   = TRUE,
                ypct     = FALSE,
                cl       = NULL,
                sd       = NULL,
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
  target <- eval(substitute(target), data, parent.frame())

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

  # Make facet variable
  if (is.null(facet)) {
    facet <- 1
  } else {
    if (!is.list(facet)) {
      facet <- list(facet)
    }

    if (length(facet) == 2 && is.null(ncol)) {
      ncol <- length(unique(facet[[2]]))
    }

    facet.order <- do.call(order, facet)
    facet       <- do.call(paste, c(facet, sep = ' | '))
    facet       <- factor(facet, levels = unique(facet[facet.order]))
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
  d    <- data.frame(x, num, den, y, facet)
  d    <- droplevels(d)
  d$cl <- ifelse(is.null(cl), NA, cl)
  d$sd <- ifelse(is.null(sd), NA, sd)

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
  d <- lapply(d, chart.fun, screenms)
  d <- lapply(d, runs.analysis)
  d <- do.call(rbind, args = c(d, make.row.names = FALSE))

  # Sigma signal
  d$sigma.signal             <- d$y < d$lcl | d$y > d$ucl
  d$sigma.signal[!d$include] <- FALSE

  # Censor control limits to ylim argument.
  if (ypct) {
    ylim[1] <- 0
    ylim[2] <- 1
    ylab    <- NULL
  }

  if (!is.null(ylim) && chart != 'run') {
    d$lcl <- pmax(d$lcl, ylim[1], na.rm = TRUE)
    d$ucl <- pmin(d$ucl, ylim[2], na.rm = TRUE)
  }

  # Remove control limits from empty subgroups.
  d$lcl[is.na(d$y)] <- NA
  d$ucl[is.na(d$y)] <- NA

  # Multiply y coordinates if needed.
  d$y   <- d$y * multiply
  d$cl  <- d$cl * multiply
  d$lcl <- d$lcl * multiply
  d$ucl <- d$ucl * multiply
  d$target <- if (is.null(target)) NA_integer_ else target

  d <- d[order(d$facet, d$part, d$x),]

  d <- d[c('facet', 'part', 'x', 'num', 'den', 'y', 'target',
           'longest.run', 'longest.run.max', 'n.crossings', 'n.crossings.min',
           'lcl', 'cl', 'ucl', 'runs.signal', 'sigma.signal',
           'freeze', 'include', 'base', 'n.obs', 'n.useful')]

  d <- list(title    = title,
            xlab     = xlab,
            ylab     = ylab,
            ncol     = ncol,
            yfixed   = yfixed,
            ypct     = ypct,
            freeze   = freeze,
            split    = split,
            exclude  = exclude,
            chart    = chart,
            data     = d)
# return(d)
  # Draw plot.
  if (plot) {
    plot.pbc(d)
  }

  # Make data frame an 'spc' object and return invisibly.
  class(d) <- c('pbc', class(d))
  invisible(d)
}
