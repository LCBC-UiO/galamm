#' Extract Model Fitted Values
#'
#' @param object galamm object
#' @param ... other arguments
#'
#' @return Numerical vector
#' @export
#'
fitted.galamm <- function(object, ...){
  object$fit
}
