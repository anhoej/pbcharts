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
  # Get data -------------------------------------------------------------------
  d      <- x$data
  freeze <- x$freeze
  yfixed <- x$yfixed
  parts  <- unique(d$part)

  # Prepare canvas -------------------------------------------------------------
  # Get axis ranges.
  ylim <- range(d$y,
                d$lcl,
                d$ucl,
                na.rm = TRUE)

  xlim <- range(d$x)

  # Get x axis function.
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
  mfrow    <- c(n_rows, n_cols)

  # Get indices of outer plots for axis tick labels.
  outer_y  <- seq(1, n_facets, by = n_cols)
  outer_x  <- seq(n_facets - n_cols + 1, n_facets)

  # Set colours and graphical parameters.
  col1 <- 'steelblue'
  col2 <- 'grey30'
  col3 <- 'tomato'
  col4 <- 'gray'

  cex.adj <- 0.9

  op <- graphics::par(
    mfrow    = mfrow,
    xpd      = NA,
    mar      = c(1.5,
                 0.7,
                 ifelse(n_facets == 1, 0, 1.5),
                 ifelse(yfixed, 1, 2.7)),
    oma      = c(2.5,
                 4.1,
                 ifelse(is.null(x$title), 1, 2.6),
                 0.5),
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

  # Draw facets ----------------------------------------------------------------
  d <- split(d, d$facet)
  j <- 0
  for(i in d) {
    j <- j + 1

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
         axes = FALSE,
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
    graphics::abline(v = mean(c(i$x[freeze], i$x[freeze + 1])),
                     lty = 3)

    # Draw parts
    for(p in parts) {
      ip <- i[i$part == p,]

      # Set colours and centre line types.
      dotcol                  <- ifelse(ip$include, col1, col4)
      dotcol[ip$sigma.signal] <- col3
      dotcol[ip$y == ip$cl]   <- col4

      clcol                   <- ifelse(ip$runs.signal[1],
                                        col3,
                                        col2)
      cltyp                   <- ifelse(ip$runs.signal[1],
                                        'dashed',
                                        'solid')

      graphics::lines(ip$x, ip$cl,         # centre line
                      col = clcol,
                      lty = cltyp)
      graphics::lines(ip$x, ip$lcl,        # lower control limit
                      col = col2)
      graphics::lines(ip$x, ip$ucl,        # upper control limit
                      col = col2)
      graphics::lines(ip$x, ip$y,          # data line
                      col = col1,
                      lwd = 2.5)
      graphics::points(ip$x, ip$y,         # data points
                       cex = ifelse(dotcol == col1, 0.8, 1),
                       col = dotcol,
                       pch = 19)
      graphics::text(max(ip$x), ip$cl[1],  # centre line label
                     labels = formatC(i$cl[1], digits = 2, format = 'fg'),
                     adj    = -0.2,
                     las    = 1,
                     cex    = 0.7)
    }

    # Add facet labels
    if (n_facets > 1)
      graphics::title(main      = i$facet[1],
                      adj       = 0,
                      font.main = 1,
                      line      = 0.8)
  }

  # Finish plot ----------------------------------------------------------------
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
}
