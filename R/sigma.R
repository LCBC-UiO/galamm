#' Extract square root of dispersion parameter from galamm object
#'
#' Extracts the square root of the dispersion parameter(s) from an object of
#' class \code{galamm}, returned from \code{\link{galamm}}. In the case of
#' conditionally Gaussian responses, this is the residual standard deviation.
#' When there are multiple dispersion parameters, e.g., with mixed response
#' type models, the square root of all of them are returned in a numeric vector.
#'
#' @param object An object of class \code{galamm}.
#' @param ... Other optional arguments. Currently not used.
#'
#' @return The square root of one or more dispersion parameters.
#'
#' @seealso [galamm()]
#' @export
#'
#' @family {details of model fit}
#'
#' @examples
#' # Linear mixed model with heteroscedastic residuals
#' mod <- galamm(
#'   formula = y ~ x + (1 | id),
#'   weights = ~ (1 | item),
#'   data = hsced
#' )
#'
#' # Extract residual standard deviation.
#' sigma(mod)
#'
#' # The residual standard deviation applies to the base case. The variance
#' # function shown in the model output shows the estimated multiplier for
#' # various grouping levels:
#' summary(mod)
#'
sigma.galamm <- function(object, ...) {
  sqrt(object$parameters$dispersion_parameter)
}
