#' Summarise pbc object
#'
#' @param object A 'pbc' object.
#' @param ... For compatibility with generic summary function.
#'
#' @returns
#' @export
#'
#' @examples
summary.pbc <- function(object, ...) {
  d      <- object$data
  freeze <- object$freeze

  if (is.null(freeze))
    freeze <- NA

  # freeze <- x$freeze
  d <- split(d, d$facets)
  d <- lapply(d, function(x) {
    data.frame(facet = x$facet[1],
               n            = nrow(x),
               n.useful     = sum(x$useful),
               freeze       = freeze,
               avg_lcl      = mean(x$lcl, na.rm = TRUE),
               cl           = mean(x$cl, na.rm = TRUE),
               avg_ucl      = mean(x$ucl, na.rm = TRUE),
               sigma.signal = sum(x$sigma.signal, na.rm = TRUE),
               runs.signal  = as.integer(any(x$runs.signal)))
  })
  do.call(rbind, c(d, make.row.names = F))
}
