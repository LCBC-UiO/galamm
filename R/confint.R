#' @title Confidence intervals for model parameters
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.3a} match.arg() used on "method" argument.
#' @srrstats {G2.3b} Arguments parm and method are case sensitive, as stated in
#'   their documentation.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param parm Parameters for which to compute intervals. Use \code{"theta"} to
#'   get all variance parameters, \code{"beta"} to get all fixed regression
#'   coefficients, \code{"lambda"} to get all factor loadings, and
#'   \code{"weights"} to get all weights. The parameter can also be given as a
#'   numeric vector with indices specifying the parameters. When given as
#'   characters, the arguments are case sensitive.
#' @param level Decimal number specifying the confidence level. Defaults to 0.95.
#' @param method Character of length one specifying the type of confidence
#'   interval. Currently only "Wald" is available. The argument is case
#'   sensitive.
#' @param ... Other arguments passed on to other methods. Currently not used.
#'
#' @return A matrix with the requested confidence intervals.
#' @export
#'
#'
#' @seealso [fixef.galamm()] for fixed effects, [coef.galamm()] for coefficients
#'   more generally, and [vcov.galamm()] for the variance-covariance matrix.
#'   [confint()] is the generic function.
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
#' confint(count_mod, parm = "beta", level = .99)
#'
confint.galamm <- function(object, parm, level = 0.95,
                           method = "Wald", ...) {
  method <- match.arg(method, "Wald")
  stopifnot(length(level) == 1 && level > 0 && level < 1)

  inds <- find_parm_inds(object, parm)

  cf <- object$parameters$parameter_estimates[inds]
  ses <- sqrt(diag(vcov(object, parm)))

  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  fac <- stats::qnorm(a)
  pct <- paste(
    format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%"
  )
  ci <- array(NA_real_,
    dim = c(length(inds), 2L),
    dimnames = list(inds, pct)
  )
  ci[] <- cf + ses %o% fac
  rownames(ci) <- object$parameters$parameter_names[inds]
  ci
}
