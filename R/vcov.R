#' @title Calculate variance-covariance matrix for GALAMM fit
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.3,G2.3b} Argument "parm" is case sensitive, as is documented here.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#' @srrstats {RE4.6} The variance-covariance matrix of the model parameters (via vcov())
#'
#' @param object Object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param parm The parameters for which the variance-covariance matrix should be
#'   calculated. Character vector with one or more of the elements "theta",
#'   "beta", "lambda", and "weights". Can also be an integer vector. When given
#'   as a character, it must be in only lowercase letters.
#' @param ... Further arguments passed on to other methods. Currently not used.
#'
#' @return A variance-covariance matrix.
#' @export
#'
#' @seealso [confint.galamm()] for the method computing confidence intervals.
#'   See [vcov()] for the generic function.
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
#' # Extract covariance matrix for fixed regression coefficients
#' vcov(mod, parm = "beta")
#'
#' # and then for weights, which gives us the variance.
#' vcov(mod, parm = "weights")
#'
vcov.galamm <- function(object, parm = "beta", ...) {
  inds <- find_parm_inds(object, parm)
  if (length(inds) == 0) {
    stop("Parameter not found.")
  }
  if (object$model$reduced_hessian) {
    inds <- inds - max(object$parameters$theta_inds)
  }

  if (qr(object$model$hessian)$rank < ncol(object$model$hessian)) {
    warning(
      "Rank deficient Hessian matrix.",
      "Could not compute covariance matrix.\n"
    )
    (NA * object$model$hessian)[inds, inds, drop = FALSE]
  } else {
    -solve(object$model$hessian)[inds, inds, drop = FALSE]
  }
}


#' Find vector indices corresponding to parameter type
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param parm Either an integer vector, in which it is simply returned, or
#'   a character vector with one or more of the elements \code{"theta"},
#'   \code{"beta"}, \code{"lambda"}, and \code{"weights"}.
#'
#' @srrstats {G1.4a} Internal function documented.
#'
#' @return A numeric vector of indices for the parameters.
#' @noRd
#'
#' @examples
#' mod <- galamm(
#'   formula = y ~ x + (1 | id),
#'   weights = ~ (1 | item),
#'   data = hsced
#' )
#'
#' # The single Cholesky factor parameter is at the first location
#' find_parm_inds(mod, "theta")
#'
#' # Regression coefficients are at location 2 and 3
#' find_parm_inds(mod, "beta")
#'
#' # Lambda is empty since there are no factor loadings
#' find_parm_inds(mod, "lambda")
#'
#' # Weight index is at location 4
#' find_parm_inds(mod, "weights")
find_parm_inds <- function(object, parm) {
  if (is.integer(parm)) {
    parm
  } else if (is.character(parm)) {
    Reduce(function(x, y) {
      c(x, eval(parse(text = paste0("object$parameters$", y, "_inds"))))
    }, parm, init = integer())
  } else {
    stop("parm must be an integer or character vector")
  }
}
