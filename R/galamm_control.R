new_galamm_control <- function(fnscale = -1, lmm = 20, trace = 3) {
  structure(trace, class = "galamm_control")
}

validate_galamm_control <- function(x) {
  x
}

#' Create new galamm_control object
#'
#' @param trace Verbosity. Passed onto \code{\link{stats::optim}}
#' @param lmm Number of BFGS updates retained in the L-BFGS-B method.
#'
#' @return Object of class galamm_control
#' @export
#'
galamm_control <- function(trace = 3L, lmm = 20) {
  validate_galamm_control(
    new_galamm_control(trace = trace, lmm = lmm)
  )
}
