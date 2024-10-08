#' @title Extract galamm coefficients
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#' @srrstats {RE4.2} Model coefficients (via coef() / coefficients())
#'
#' @description
#' Currently, this function only returns the fixed effects.
#'
#' @param object An object of class \code{galamm}, from \code{\link{galamm}}.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#' @return A matrix with the requested coefficients.
#' @export
#'
#'
#' @seealso [fixef.galamm()] for fixed effects, [ranef.galamm()] for random
#'   effects, and [coef()] for the generic function.
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
#' # Extract coefficients
#' coef(count_mod)
#'
coef.galamm <- function(object, ...) {
  fixef(object)
}
