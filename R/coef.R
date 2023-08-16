#' Extract galamm Coefficients
#'
#' @param object An object of class \code{\link{galamm}}
#' @param ... Additional parameters
#'
#' @return An object
#' @export
#'
coef.galamm <- function(object, ...) {
  fixef(object)
}
