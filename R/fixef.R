##' @importFrom nlme fixef
##' @export fixef
NULL

#' @title Extract fixed effects from galamm objects
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#'
#' @description
#' Extract the fixed regression coefficients.
#'
#' @param object An object of class \code{galamm}, returned from
#'   \code{\link{galamm}}.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#'
#' @return A named \code{numeric} vector containing the requested fixed effects.
#'
#' @name fixef
#' @aliases fixef fixef.galamm
#' @export
#'
#' @seealso [ranef.galamm()] for random effects, [coef.galamm()] for
#' coefficients more generally, and [confint.galamm()] for confidence intervals.
#'
#' @family details of model fit
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
