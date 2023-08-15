#' Control Values for galamm Fit
#'
#' @param trace Verbosity. Passed onto \code{\link{stats::optim}}
#' @param lmm Number of BFGS updates retained in the L-BFGS-B method. Passed
#'   onto \code{\link{stats::optim}}.
#' @param maxit_conditional_modes Maximum number of iterations in penalized
#'   iteratively reweighted least squares algorithm. Ignored if \code{family =
#'   "gaussian"} for all observations, since then a single step gives the exact answer.
#'
#' @return A list
#' @export
#'
galamm_control <- function(trace = 0L, lmm = 20, maxit_conditional_modes = 10){

  if(length(trace) != 1L && trace < 0){
    stop("trace should be a non-negative integer of length one")
  }

  stopifnot(length(lmm) == 1 && lmm > 0)
  stopifnot(length(maxit_conditional_modes) == 1 &&
              maxit_conditional_modes > 0)
  list(
    fnscale = -1,
    trace = trace,
    lmm = lmm,
    maxit_conditional_modes = maxit_conditional_modes
  )
}
