##' @importFrom nlme fixef
##' @export fixef
NULL

#' Extract fixed effects from galamm objects
#'
#' Extract the fixed regression coefficients.
#'
#' @param object An object of class \code{galamm}, returned from
#'   \code{\link{galamm}}.
#' @param ... Optional parameters passed on to other functions. Currently
#' not used.
#'
#' @return A matrix with the requested fixed effects.
#'
#' @name fixef
#' @aliases fixef fixef.galamm
#' @export
#'
#' @seealso [ranef.galamm()] for random effects, [coef.galamm()] for
#' coefficients more generally, and [confint.galamm()] for confidence intervals.
fixef.galamm <- function(object, ...) {
  ret <- object$par[object$beta_inds]
  names(ret) <- object$par_names[object$beta_inds]
  ret
}
