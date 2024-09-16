#' @title Extract response values
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#' @srrstats {RE4.8} Response variables, and associated "metadata" where
#'   applicable.
#'
#' @description Extracts response values from a model.
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#'
#' @return A numerical vector with fit response values for each row in the input
#'   data.
#' @export
#'
#' @family details of model fit
#'
#' @examples
#' # Linear mixed model with heteroscedastic residuals
#' mod <- galamm(
#'   formula = y ~ x + (1 | id),
#'   weights = ~ (1 | item),
#'   data = hsced
#' )
#'
#' # Plot response versus fitted values
#' plot(fitted(mod), response(mod))
#'
response <- function(object, ...) {
  object$model$response
}
