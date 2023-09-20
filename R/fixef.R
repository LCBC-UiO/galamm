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
#'
#' @family {details of model fit}
#'
#' @examples
#' # Poisson GLMM
#' count_mod <- galamm(
#'   formula = y ~ lbas * treat + lage + v4 + (1 | subj),
#'   data = epilep, family = poisson
#' )
#'
#' # Extract fixed effects
#' fixef(count_mod)
#'
fixef.galamm <- function(object, ...) {
  ret <- object$parameters$parameter_estimates[object$parameters$beta_inds]
  names(ret) <- object$parameters$parameter_names[object$parameters$beta_inds]
  ret
}
