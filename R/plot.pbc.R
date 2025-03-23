#' Plot process behaviour chart
#'
#' @param x List of class 'pbc'.
#' @param ... Unused, necessary for generic function.
#'
#' @returns NULL, plots a process behaviour chart.
#' @export
#'
#' @examples
#' p <- pbc(rnorm(12), plot = FALSE)
#' plot(p)
#'
plot.pbc <- function(x, ...) {
  # Begin prepare canvas -------------------------------------------------------
  # Get data.
  d           <- x$data
  freeze      <- x$freeze
  yfixed <- x$yfixed

  # Get indices of phase 1 period (<= freeze).
  if (is.null(freeze)) {
    base <- seq_along(d$x)
  } else {
    base <- seq_len(freeze)
  }

  # Get axis ranges.
  ylim <- range(d$y,
                d$lcl,
                d$ucl,
                na.rm = TRUE)

  xlim <- range(d$x)

  # Get x axis class
  if (inherits(d$x, 'Date')) {
    x_class <- graphics::axis.Date
  } else if (inherits(d$x, 'POSIXct')) {
    x_class <- graphics::axis.POSIXct
  } else {
    x_class <- graphics::axis
  }

  # Calculate facets layout.
  n_facets <- length(unique(d$facet))
  n_cols   <- ifelse(is.null(x$ncol),
                     floor(sqrt(n_facets)),
                     x$ncol)
  n_rows   <- ceiling(n_facets / n_cols)
  outer_y <- seq(1, n_facets, by = n_cols)
  outer_x <- seq(n_facets - n_cols + 1, n_facets)

  # Set colours and graphical parameters.
  col1 <- 'steelblue'
  col2 <- 'grey30'
  col3 <- 'tomato'
  col4 <- 'gray'

  cex.adj <- 0.9

  op <- graphics::par(
    mfrow    = c(n_rows, n_cols),
    mar      = c(1.5, 1.2, ifelse(n_facets == 1, 0, 2), 3),
    oma      = c(2.5, 4.1, ifelse(is.null(x$title), 1, 2.6), 0),
    cex      = cex.adj,
    cex.axis = cex.adj,
    cex.main = cex.adj,
    cex.lab  = cex.adj
  )

  on.exit(graphics::par(op))

  axis_par <- list(las       = 1,
                   lwd       = 0,
                   lwd.ticks = 1,
                   tcl       = -0.2,
                   col       = col2)
  # End prepare canvas ---------------------------------------------------------

  # Begin draw facets ----------------------------------------------------------
  d <- split(d, d$facet)
  j <- 0
  for(i in d) {
    j <- j + 1
    # Set colours and centre line types.
    dotcol                 <- ifelse(i$useful, col1, col4)
    dotcol[i$sigma.signal] <- col3
    clcol                  <- ifelse(i$runs.signal[1],          # phase 1
                                     col3,
                                     col2)
    cltyp                  <- ifelse(i$runs.signal[1],
                                     'dashed',
                                     'solid')
    clcol2                 <- ifelse(i$runs.signal[freeze + 1],  # phase 2
                                     col3,
                                     col2)
    cltyp2                 <- ifelse(i$runs.signal[freeze + 1],
                                     'dashed',
                                     'solid')

    # Free y axis scales if yfixed argument is FALSE
    if (!yfixed) {
      ylim <- range(i$y,
                    i$lcl,
                    i$ucl,
                    na.rm = TRUE)
    }

    # Prepare canvas.
    plot(i$x, i$y,
         type = 'n',
         axes = F,
         ylim = ylim,
         xlim = xlim,
         ylab = '',
         xlab = '')

    # Add box and axes.
    graphics::box(bty = 'l', col = col2)

    do.call(x_class,                            # x axis
            c(1, axis_par,
              labels = ifelse(j %in% outer_x,
                              TRUE,
                              FALSE)))

    do.call(graphics::axis,                     # y axis
            c(2, axis_par,
              labels = ifelse(j %in% outer_y | !yfixed,
                              TRUE,
                              FALSE)))

    # Add lines and points.
    graphics::lines(i$x[base], i$cl[base],      # centre line
                    col = clcol,
                    lty = cltyp)
    graphics::lines(i$x[-base], i$cl[-base],
                    col = clcol2,
                    lty = cltyp2)
    graphics::lines(i$x[base], i$lcl[base],     # lower control limit
                    col = col2)
    graphics::lines(i$x[-base], i$lcl[-base],
                    col = col2)
    graphics::lines(i$x[base], i$ucl[base],     # upper control limit
                    col = col2)
    graphics::lines(i$x[-base], i$ucl[-base],
                    col = col2)
    graphics::lines(i$x, i$y,                   # data line
                    col = col1,
                    lwd = 2.5)
    graphics::points(i$x, i$y,                  # data points
                     cex = 0.8,
                     col = dotcol,
                     pch = 19)

    # Add part labels.
    if (!is.null(freeze)) {
      graphics::mtext(x$partlabs[1],                                # phase 1
                      at   = mean(c(as.numeric(i$x[1]),
                                    as.numeric(i$x[x$freeze]))),
                      cex  = 0.7,
                      line = -0.3)
      graphics::mtext(x$partlabs[2],                                # phase 2
                      at   = mean(c(as.numeric(i$x[x$freeze + 1]),
                                    as.numeric(max(i$x)))),
                      cex  = 0.7,
                      line = -0.3)
      # abline(v = mean(c(as.numeric(i$x[freeze]),
      #                   as.numeric(i$x[freeze + 1]))),
      #        lty = 'dotted')
    }

    # Add centre line label.
    graphics::mtext(formatC(i$cl[1], digits = 2, format = 'fg'),
                    side = 4,
                    at   = i$cl[1],
                    adj  = 0,
                    las  = 1,
                    cex  = 0.7)

    # Add facet labels
    if (n_facets > 1)
      graphics::title(main      = i$facet[1],
                      adj       = 0,
                      font.main = 1,
                      line      = 1.2)
  }
  # End draw facets ------------------------------------------------------------

  # Begin finish plot ----------------------------------------------------------
  graphics::mtext(x$xlab,           # x axis label
                  side  = 1,
                  line  = 1.3,
                  cex   = cex.adj,
                  outer = TRUE)
  graphics::mtext(x$ylab,           # y axis label
                  side  = 2,
                  line  = 3,
                  cex   = cex.adj,
                  outer = TRUE)
  graphics::mtext(x$title,          # main title
                  side  = 3,
                  line  = 1.3,
                  outer = TRUE,
                  font  = 1,
                  adj   = 0)
  # End finish plot --------------------------------------------------------------
}
