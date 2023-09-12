#' Extract model fitted values
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#' @method fitted galamm
#'
#' @return A numerical vector with fit values for each row in the input data.
#' @export
#'
fitted.galamm <- function(object, ...) {
  object$fit
}
