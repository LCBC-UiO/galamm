#' Extract model fitted values
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#'
#' @return A numerical vector with fit values for each row in the input data.
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
#' # Extract fitted values and plot against x
#' plot(hsced$x, fitted(mod))
#'
fitted.galamm <- function(object, ...) {
  object$model$fit
}
