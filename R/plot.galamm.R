#' @title Diagnostic plots for galamm objects
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
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
#' @family summary functions
#'
#' @examples
#' # Linear mixed model example from lme4
#' data("sleepstudy", package = "lme4")
#' mod <- galamm(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'
#' # Diagnostic plot
#' plot(mod)
#'
plot.galamm <- function(x, ...) {
  plot(fitted(x), residuals(x, type = "pearson"),
    xlab = "Predicted values",
    ylab = "Pearson residuals",
    ...
  )
  graphics::abline(h = 0, col = "blue")
}
