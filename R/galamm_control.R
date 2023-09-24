#' Control values for galamm fit
#'
#' This function can be called for controling the optimization procedure used
#' when fitting GALAMMs using \code{\link{galamm}}.
#'
#' @param optim_control List passed on to \code{stats::optim}'s \code{control}
#'   argument. If not otherwise specified, the following arguments are set to
#'   non-default values: \code{fnscale = -1} and \code{lmm = 20}.
#'
#' @param maxit_conditional_modes Maximum number of iterations in penalized
#'   iteratively reweighted least squares algorithm. Ignored if \code{family =
#'   "gaussian"} for all observations, since then a single step gives the exact
#'   answer.
#'
#' @param reduced_hessian Logical value. Defaults to \code{TRUE}, which means
#'   that the full Hessian matrix at the maximum marginal likelihood solution is
#'   computed. If \code{FALSE}, a reduced Hessian matrix with second order
#'   partial derivatives with respect to fixed regression coefficients and
#'   factor loadings. The latter can help is the full Hessian is not positive
#'   definite.
#'
#' @return Object of class \code{galamm_control}, which typically will be
#'   provided as an argument to \code{\link{galamm}}.
#' @export
#'
#' @seealso [galamm()]
#'
#' @family modeling functions
#'
#' @examples
#' # Define control object with quite a high degree of verbosity (trace = 6)
#' # and using the last 20 BFGS updates to estimate the Hessian in L-BFGS-B.
#' control <- galamm_control(optim_control = list(trace = 6, lmm = 20))
#'
galamm_control <- function(optim_control = list(),
                           maxit_conditional_modes = 10,
                           reduced_hessian = FALSE) {
  if ("trace" %in% names(optim_control)) {
    if (length(optim_control$trace) != 1L || any(optim_control$trace < 0)) {
      stop("trace should be a non-negative integer of length one")
    }
  }

  stopifnot(length(maxit_conditional_modes) == 1 && maxit_conditional_modes > 0)

  if ("fnscale" %in% names(optim_control)) {
    if (optim_control$fnscale > 0) {
      stop("fnscale parameter should be negative.")
    }
  }

  optim_control_names <- c(
    "trace", "fnscale", "parscale", "ndeps", "maxit",
    "abstol", "reltol", "alpha", "beta", "gamma",
    "REPORT", "warn.1d.NelderMead", "type", "lmm",
    "factr", "pgtol", "tmax", "temp"
  )

  if (any(!names(optim_control) %in% optim_control_names)) {
    stop("Unknown control names ", paste(
      setdiff(names(optim_control), optim_control_names),
      collapse = ", "
    ))
  }

  new_galamm_control(
    optim_control = optim_control,
    maxit_conditional_modes = maxit_conditional_modes,
    reduced_hessian = reduced_hessian
  )
}


#' Constructor method for galamm_control objects
#'
#' @inherit galamm_control
#' @noRd
#' @seealso [galamm_control()]
new_galamm_control <- function(optim_control, maxit_conditional_modes,
                               reduced_hessian) {
  if (is.null(optim_control$fnscale)) {
    optim_control$fnscale <- -1
  }

  if (is.null(optim_control$lmm)) {
    optim_control$lmm <- 20
  }


  ret <- list(
    maxit_conditional_modes = maxit_conditional_modes,
    reduced_hessian = reduced_hessian,
    optim_control = optim_control
  )

  class(ret) <- "galamm_control"
  ret
}
