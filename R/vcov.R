#' Calculate variance-covariance matrix for GALAMM fit
#'
#' @param object Object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param parm The parameters for which the variance-covariance matrix should be
#'   calculated. Character vector with one or more of the elements "theta",
#'   "beta", "lambda", and "weights". Can also be an integer vector.
#' @param ... Further arguments passed on to other methods. Currently not used.
#'
#' @return A variance-covariance matrix.
#' @export
#'
#' @seealso [confint.galamm()] for the method computing confidence intervals.
#'   See [vcov()] for the generic function.
vcov.galamm <- function(object, parm = "beta", ...) {
  inds <- find_parm_inds(object, parm)

  if (qr(object$hessian)$rank < ncol(object$hessian)) {
    warning("Rank deficient Hessian matrix. Could not compute covariance matrix.")
    (NA * object$hessian)[inds, inds, drop = FALSE]
  } else {
    -solve(object$hessian)[inds, inds, drop = FALSE]
  }
}


find_parm_inds <- function(object, parm) {
  if (is.integer(parm)) {
    parm
  } else if (is.character(parm)) {
    Reduce(function(x, y) {
      c(x, eval(parse(text = paste0("object$", y, "_inds"))))
    }, parm, init = integer())
  } else {
    stop("parm must be an integer or character vector")
  }
}
