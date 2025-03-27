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
  d       <- object$data
  freeze  <- object$freeze
  exclude <- object$exclude

  if (is.null(freeze))
    freeze <- NA
  d <- split(d, list(d$facet, d$phase))
  d <- lapply(d, function(x) {
    data.frame(facet        = x$facet[1],
               phase        = x$phase[1],
               n            = nrow(x),
               n.useful     = sum(x$useful, na.rm = TRUE) - length(exclude),
               avg_lcl      = mean(x$lcl, na.rm = TRUE),
               cl           = mean(x$cl, na.rm = TRUE),
               avg_ucl      = mean(x$ucl, na.rm = TRUE),
               sigma.signal = sum(x$sigma.signal, na.rm = TRUE),
               runs.signal  = as.integer(any(x$runs.signal)))
  })
  do.call(rbind, c(d, make.row.names = FALSE))
}
