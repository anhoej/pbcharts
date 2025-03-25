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
#'               baseline period (phase 1).
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
#'                        labels.
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
                exclude  = NULL,
                multiply = 1,
                ncol     = NULL,
                ylim     = NULL,
                yfixed   = TRUE,
                title    = NULL,
                xlab     = 'Subgroup',
                ylab     = 'Value',
                partlabs = c('phase 1', 'phase 2'),
                plot     = TRUE) {
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

  # Make sure that the num and den vectors are of the same length.
  if (length(den) == 1) {
    den <- rep(den, length(num))
  }

  # Make sure that numerators and denominators are balanced. If one is missing,
  # the other should be missing too.
  xna    <- !stats::complete.cases(num, den)
  num[xna] <- NA
  den[xna] <- NA

  # Ignore invalid freeze argument.
  if (!is.null(freeze) && (freeze < 2 || freeze > length(unique(x)) - 2)) {
    freeze <- NULL
    message('Invalid freeze argument, ignoring.')
  }

  # Ignore invalid exclude argument
  if (!is.null(exclude) && (exclude > length(unique(x)) || exclude < 1)) {
    exclude <- NULL
    message('Invalid exclude argument, ignoring.')
  }

  # Make y values to plot.
  y <- num / den

  # Make dummy facet if facet is null
  if (is.null(facet)) {
    facet <- rep('1', length(num))
  }

  # Make data frame.
  d <- data.frame(x, num, den, y, facet)

  # Calculate sigma limits and perform runs analysis to each facet.
  d <- split(d, facet)
  d <- lapply(d, chart.fun, freeze, exclude)
  d <- do.call(rbind, args = c(d, make.row.names = FALSE))

  # Censor control limits to ylim argument.
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

  d <- list(title    = title,
            xlab     = xlab,
            ylab     = ylab,
            ncol     = ncol,
            yfixed   = yfixed,
            freeze   = freeze,
            exclude  = exclude,
            partlabs = partlabs,
            data     = d)

  # Draw plot.
  if (plot) {
    plot.pbc(d)
  }

  # Make data frame an 'spc' object and return invisibly.
  class(d) <- c('pbc', class(d))
  invisible(d)
}
