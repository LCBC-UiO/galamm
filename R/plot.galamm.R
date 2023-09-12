#' Diagnostic plots for galamm objects
#'
#' @param x An object of class \code{galamm} returned from \code{\link{galamm}}.
#' @param ... Optional arguments passed on to the \code{plot} function.
#'
#' @return A plot is displayed.
#' @export
#'
#' @seealso [residuals.galamm()] for extracting residuals and [plot()] for the
#'   generic function.
#'
plot.galamm <- function(x, ...) {
  plot(fitted(x), residuals(x, type = "pearson"),
    xlab = "Predicted values",
    ylab = "Pearson residuals",
    ...
  )
  graphics::abline(h = 0, col = "blue")
}
