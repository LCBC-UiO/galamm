#' Extract Log-Likelihood of galamm Object
#'
#' @param object Object
#' @param ... Other arguments
#'
#' @return Object of class \code{logLik}
#' @export
#'
#' @method logLik galamm
#'
#' @seealso [deviance.galamm()] for a function returning deviance and
#' [logLik()] for the generic function.
logLik.galamm <- function(object, ...) {
  structure(
    object$loglik,
    nobs = object$n,
    df = object$df,
    class = "logLik"
  )
}


#' Extract deviance of galamm object
#'
#' @param object Object of class \code{galamm}, returned from
#'   \code{\link{galamm}}.
#' @param ... Other arguments passed on to other methods. Currently not used.
#'
#' @return A numeric value giving the deviance of the model fit.
#' @export
#'
#' @method deviance galamm
#'
#' @seealso [logLik.galamm()] for a function returning the log likelihood and
#'   [deviance()] for the generic function.
#'
deviance.galamm <- function(object, ...) {
  object$deviance
}
