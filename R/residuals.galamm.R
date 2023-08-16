#' Residuals of galamm Objects
#'
#' @param object A galamm object
#' @param type Type
#' @param ... Other arguments
#'
#' @return Vector
#' @export
#'
residuals.galamm <- function(object, type = c("pearson", "deviance"), ...) {
  type <- match.arg(type, c("pearson", "deviance"))
  if (type == "pearson"){
    object$pearson_residuals
  } else if (type == "deviance") {
    object$deviance_residuals
  }
}
