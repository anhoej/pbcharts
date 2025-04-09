#' Summarise pbc object
#'
#' @param object A 'pbc' object.
#' @param ... For compatibility with generic summary function.
#'
#' @returns A data frame summarising each facets of a pcb object.
#' @export
#'
#' @examples
#' p <- pbc(rnorm(12), chart = 'i', plot = FALSE)
#' summary(p)
summary.pbc <- function(object, ...) {
  d <- object$data

  d <- split(d, ~ facet + part)
  d <- lapply(d, function(x) {
    data.frame(facet           = x$facet[1],
               part            = x$part[1],
               n.obs           = x$n.obs[1],
               n.useful        = x$n.useful[1],
               avg_lcl         = mean(x$lcl, na.rm = TRUE),
               cl              = mean(x$cl, na.rm = TRUE),
               avg_ucl         = mean(x$ucl, na.rm = TRUE),
               sigma.signal    = sum(x$sigma.signal, na.rm = TRUE),
               runs.signal     = as.integer(any(x$runs.signal)),
               longest.run     = x$longest.run[1],
               longest.run.max = x$longest.run.max[1],
               n.crossings     = x$n.crossings[1],
               n.crossings.min = x$n.crossings.min[1])
  })
  do.call(rbind, c(d, make.row.names = FALSE))
}
