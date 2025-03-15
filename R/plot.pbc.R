#' Plot process behaviour chart
#'
#' @param x List of class 'pbc'.
#' @param ... Unused, necessary for generic function.
#'
#' @returns Plots a process behaviour chart.
#' @export
#'
#' @examples
#' p <- pbc(rnorm(12), plot = FALSE)
#' plot(p)
plot.pbc <- function(x, ...) {
  d <- x$data

  # To silence the "no visible binding" warning.
  freeze <- NULL

  col1 <- 'steelblue'
  col2 <- 'grey30'
  col3 <- 'tomato'
  col4 <- 'gray'

  if (is.null(x$freeze)) {
    base <- seq_along(d$x)
  } else {
    base <- seq_len(x$freeze)
  }

  # Make room for data and control limits on the y axes.
  ylim <- range(d$y,
                d$lcl,
                d$ucl,
                na.rm = TRUE)

  # Find x axis format.
  if (inherits(d$x, 'Date')) {
    x_class <- graphics::axis.Date
  } else if (inherits(d$x, 'POSIXct')) {
    x_class <- graphics::axis.POSIXct
  } else {
    x_class <- graphics::axis
  }

  # Calculate plots layout.
  n_facets <- length(unique(d$facet))
  n_cols   <- ifelse(is.null(x$ncol),
                     ceiling(sqrt(n_facets)),
                     ncol)
  n_rows   <- ceiling(n_facets / n_cols)

  # Prepare plot canvas.
  op <- graphics::par(mfrow = c(n_rows, n_cols),
                      mar   = c(3, 2, ifelse(n_facets == 1, 0, 2), 3),
                      oma   = c(2, 2.6, 3.5, 0))
  on.exit(graphics::par(op))
  axis_par <- list(las       = 1,
                   lwd       = 0,
                   lwd.ticks = 1,
                   tcl       = -0.3,
                   col       = col2)

  # Populate facet.
  d <- split(d, d$facet)
  for(i in d) {
    dotcol                 <- ifelse(i$useful, col1, col4)
    dotcol[i$sigma.signal] <- col3
    clcol                  <- ifelse(i$runs.signal[1], col3, col2)
    cltyp                  <- ifelse(i$runs.signal[1], 'dashed', 'solid')
    clcol2                 <- ifelse(i$runs.signal[x$freeze + 1],
                                     col3,
                                     col2)
    cltyp2                 <- ifelse(i$runs.signal[x$freeze + 1],
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

    if (!is.null(x$freeze)) {
      # abline(v = mean(c(as.numeric(x$x[freeze]),
      #                   as.numeric(x$x[freeze + 1]))),
      #        lty = 'dotted')
      graphics::mtext(x$partlabs[1],
                      at   = mean(c(as.numeric(d$x[1]),
                                    as.numeric(d$x[freeze]))),
                      cex  = 0.7,
                      line = -0.9)
      graphics::mtext(x$partlabs[2],
                      at   = mean(c(as.numeric(d$x[freeze] + 1),
                                    as.numeric(utils::tail(d$x, 1)))),
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
      graphics::title(main = i$facet[1], adj = 0, font.main = 1, line = 1.2)
  }

  # Main title and axis labels.
  graphics::mtext(x$xlab,
                  side  = 1,
                  line  = 0.5,
                  outer = TRUE)
  graphics::mtext(x$ylab,
                  side  = 2,
                  line  = 1,
                  outer = TRUE)
  graphics::mtext(x$title,
                  side  = 3,
                  line  = 1.3,
                  outer = TRUE,
                  font  = 1,
                  cex   = 1.2,
                  adj   = 0)

  # Reset plot canvas.
  # graphics::par(op)
}
