#' Process behaviour chart
#'
#' @param x Numeric or date(time) vector of subgroup values to plot along the x
#'         axis. Or, if y is NULL, x values will be used for y coordinates.
#' @param num Numeric vector of measures or counts to
#'            plot on the y axis (numerator).
#' @param den Numeric vector of subgroup sizes (denominator).
#' @param facet Character (or factor) vector used for faceting.
#' @param data Data frame containing the variables used in the plot.
#' @param chart Character value indicating the chart type. Possible values are:
#'             'run' (default), and 'i'.
#' @param multiply Number to multiply y axis by, e.g. 100 to get percentages rather
#'            than proportions.
#' @param ylim Numeric vector (y1, y2) setting the y axis limits. Useful e.g.
#'             for preventing negative negative control limits (c(0, NA) or
#'             proportions above 1 (100%) (c(0, 1).
#' @param ncol Number of columns in faceted plot.
#' @param freeze Integer indicating the index of the last subgroup in the
#'               baseline period (phase 1).
#' @param title Character setting the main chart title
#' @param xlab Character setting the main y axis label
#' @param ylab Character setting the main x axis label
#' @param partlabs Character vector of length two setting the labels for phase 1
#'                 and phase 2 periods.
#' @param plot Logical, if TRUE (default), plots an SPC chart.
#' @param print Logical, if TRUE, prints a data frame with coordinates.
#'
#' @returns
#' @export
#'
#' @examples
pbc <- function(x,
                num      = NULL,
                den      = 1,
                facet        = NULL,
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
  facet   <- eval(substitute(facet), data, parent.frame())

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

  # Make dummy facet if facet is null
  if (is.null(facet)) {
    facet <- rep('1', length(num))
  }

  # Make data frame.
  d <- data.frame(x, num, den, y, facet)

  # Calculate sigma limits and perform runs analysis to each facet.
  d <- split(d, facet)
  d <- lapply(d, chart.fun, freeze)
  d <- do.call(rbind, args = c(d, make.row.names = F))

  # Set y limits
  if (!is.null(ylim)) {
    d$lcl <- pmax(d$lcl, ylim[1])
    d$ucl <- pmin(d$ucl, ylim[2])
  }

  # Find useful data points (not on centre line).
  d$useful <- d$y != d$cl

  # Multiply y coordinates if needed.
  d$y   <- d$y * multiply
  d$cl  <- d$cl * multiply
  d$lcl <- d$lcl * multiply
  d$ucl <- d$ucl * multiply

  d <- list(title = title,
            ncol = ncol,
            freeze = freeze,
            xlab = xlab,
            ylab = ylab,
            partlabs = partlabs,
            data = d)

  # Make plot.
  if (plot) {
    plot.pbc(d)
  }

  # Print data frame.
  if (print) {
    print(d$data)
  }

  # Make data frame an 'spc' object and return invisibly.
  class(d) <- c('pbc', class(d))
  invisible(d)
}
