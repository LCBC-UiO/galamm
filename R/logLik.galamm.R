#' Extract Log-Likelihood of galamm Object
#'
#' @param object Object
#' @param ... Other arguments
#'
#' @return Object of class \code{logLik}
#' @export
#'
logLik.galamm <- function(object, ...){
  structure(
    object$loglik,
    nobs = object$n,
    df = object$df,
    class = "logLik")
}


#' Extract Deviance of galamm Object
#'
#' @param object Object
#' @param ... Other arguments
#'
#' @return Deviance
#' @export
#'
deviance.galamm <- function(object, ...){
  object$deviance
}
