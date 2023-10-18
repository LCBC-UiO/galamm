#' @title Extract Log-Likelihood of galamm Object
#' @srrstats {G1.4} Function documented with roxygen2.
#' @param object Object
#' @param ... Other arguments
#'
#' @return Object of class \code{logLik}
#' @export
#'
#'
#' @seealso [deviance.galamm()] for a function returning deviance and
#' [logLik()] for the generic function.
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
#' # Extract log likelihood
#' logLik(mod)
#'
logLik.galamm <- function(object, ...) {
  structure(
    object$model$loglik,
    nobs = nobs(object),
    df = object$model$df,
    class = "logLik"
  )
}


#' @title Extract deviance of galamm object
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#'
#' @param object Object of class \code{galamm}, returned from
#'   \code{\link{galamm}}.
#' @param ... Other arguments passed on to other methods. Currently not used.
#'
#' @return A numeric value giving the deviance of the model fit.
#' @export
#'
#' @seealso [logLik.galamm()] for a function returning the log likelihood and
#'   [deviance()] for the generic function.
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
#' # Extract deviance
#' deviance(mod)
#'
deviance.galamm <- function(object, ...) {
  object$model$deviance
}
