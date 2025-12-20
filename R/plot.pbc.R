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
  ypct   <- x$ypct
  parts  <- unique(d$part)

  # Set constants --------------------------------------------------------------
  col1       <- 'steelblue'  # line
  col2       <- 'grey30'     # non-data elements, e.g. axes and control limits
  col3       <- 'tomato'     # signal
  col4       <- 'gray'       # ignored datapoints
  col5       <- 'darkgreen'  # target line
  cex.adj    <- 0.9

  facet_char <- as.character(d$facet[1])
  facet_grid <- nchar(facet_char) - nchar(gsub('\\|', '', facet_char)) == 1

  # Prepare canvas -------------------------------------------------------------
  # Calculate facets layout.
  n_facets <- length(unique(d$facet))
  n_cols   <- ifelse(is.null(x$ncol),
                     floor(sqrt(n_facets)),
                     x$ncol)
  n_rows   <- ceiling(n_facets / n_cols)
  mfrow    <- c(n_rows, n_cols)

  # Get indices of outer facets for tick labels.
  outer_x   <- seq(n_facets - n_cols + 1, n_facets)
  outer_y   <- seq(1, n_facets, by = n_cols)
  outer_top <- 1:n_cols
  outer_right <- seq(n_cols, n_facets, by = n_cols)

  # Set graphical parameters
  op <- graphics::par(
    mfrow    = mfrow,
    mar      = c(1.5,
                 1.5,
                 ifelse(n_facets > 1 && !facet_grid, 1.5, 0.2),
                 ifelse(yfixed, 1, 2.7)),
    oma      = c(2.5,
                 2.8,
                 ifelse(is.null(x$title), 1, 3),
                 ifelse(facet_grid, 1.7, 0)),
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

  # Get x axis function.
  if (inherits(d$x, 'Date')) {
    x_class <- graphics::axis.Date
  } else if (inherits(d$x, 'POSIXct')) {
    x_class <- graphics::axis.POSIXct
  } else {
    x_class <- graphics::axis
  }

  # Get axis ranges.
  xlim <- range(d$x)
  ylim <- range(d$y,
                d$lcl,
                d$ucl,
                d$target,
                na.rm = TRUE)

  # Draw facets ----------------------------------------------------------------
  d <- split(d, d$facet)
  j <- 0
  for (i in d) {
    j <- j + 1

    # Get y axis range for free axis.
    if (!yfixed) {
      ylim <- range(i$y,
                    i$lcl,
                    i$ucl,
                    i$target,
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

    yticks <- graphics::axTicks(2)

    if (j %in% outer_y || !yfixed) {
      ylabs <- yticks

      if (ypct) {
        ylabs <- paste0(ylabs * 100, '%')
      }

    } else {
      ylabs <- FALSE
    }

    do.call(x_class,                            # x axis
            c(1, axis_par,
              labels = ifelse(j %in% outer_x,
                              TRUE,
                              FALSE)))

    do.call(graphics::axis,                     # y axis
            c(2, axis_par,
              list(at     = yticks,
                   labels = ylabs))
    )

    # Add lines and points.
    graphics::abline(v   = mean(c(i$x[freeze], i$x[freeze + 1])),  # freeze line
                     lty = 3)

    # Draw parts.
    for (p in parts) {
      ip <- i[i$part == p,]

      # Set colours and centre line types.
      clcol                   <- ifelse(ip$runs.signal[1], col3, col2)
      cltyp                   <- ifelse(ip$runs.signal[1], 'dashed', 'solid')
      dotcol                  <- ifelse(ip$include, col1, col4)
      dotcol[ip$sigma.signal] <- col3
      dotcol[ip$y == ip$cl]   <- col4

      # Lines and points
      graphics::lines(ip$x, ip$target,     # target line
                      col = col5,
                      lty = 'dashed')
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

      # Line labels.
      clval     <- ip$cl[1]
      lclval    <- mean(ip$lcl, na.rm = TRUE)
      uclval    <- mean(ip$ucl, na.rm = TRUE)
      targetval <- ip$target[nrow(ip)]

      graphics::text(max(ip$x), clval,      # centre line label
                     labels = format_labs(clval, ypct),
                     xpd    = NA,
                     adj    = -0.2,
                     las    = 1,
                     cex    = 0.7)

      graphics::text(max(ip$x), uclval,     # UCL label
                     labels = format_labs(uclval, ypct),
                     xpd    = NA,
                     adj    = -0.2,
                     las    = 1,
                     cex    = 0.7)

      graphics::text(max(ip$x), lclval,     # LCL label
                     labels = format_labs(lclval, ypct),
                     xpd    = NA,
                     adj    = -0.2,
                     las    = 1,
                     cex    = 0.7)

      graphics::text(max(ip$x), targetval,  # target label
                     labels = format_labs(targetval, ypct),
                     xpd    = NA,
                     adj    = -0.2,
                     las    = 1,
                     cex    = 0.7)
    }

    # Facet labels.
    if (facet_grid) {
      facet_row <- sub(' \\|.*$', '', ip$facet[1])
      facet_col <- sub('^.* \\| ', '', ip$facet[1])

      if (j %in% outer_right) {
        graphics::text(graphics::par('usr')[2],
                       mean(graphics::par('usr')[3:4]),
                       labels = facet_row,
                       xpd = NA,
                       srt = -90,
                       adj = c(0.5, -3),
                       cex = 0.8)
      }

      if (j %in% outer_top) {
        graphics::title(main      = facet_col,
                        xpd       = NA,
                        adj       = 0,
                        font.main = 1,
                        line      = 0.6)
      }

    } else if (n_facets > 1) {
      graphics::title(main      = i$facet[1],
                      adj       = 0,
                      font.main = 1,
                      line      = 0.8)
    }

  }

  # Finish plot ----------------------------------------------------------------
  graphics::mtext(x$xlab,           # x axis label
                  side  = 1,
                  line  = 1.3,
                  cex   = cex.adj,
                  outer = TRUE)
  graphics::mtext(x$ylab,           # y axis label
                  side  = 2,
                  line  = 1.7,
                  cex   = cex.adj,
                  outer = TRUE)
  graphics::mtext(x$title,          # main title
                  side  = 3,
                  line  = 1.5,
                  outer = TRUE,
                  font  = 1,
                  adj   = 0)
}
