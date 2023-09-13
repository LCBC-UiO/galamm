#' Residuals of galamm objects
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param type Type of residuals to be returned. One of \code{"pearson"} and
#'   \code{"deviance"}.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#' @return Numeric vector of residual values.
#' @export
#'
#' @seealso [fitted.galamm()] for model fitted values, [predict.galamm()] for
#' model predictions, and [plot.galamm()] for diagnostic plots. The generic
#' function is [residuals()].
#'
residuals.galamm <- function(object, type = c("pearson", "deviance"), ...) {
  type <- match.arg(type, c("pearson", "deviance"))
  if (type == "pearson") {
    object$pearson_residuals
  } else if (type == "deviance") {
    object$deviance_residuals
  }
}
