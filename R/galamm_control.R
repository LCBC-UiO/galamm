#' Control Values for galamm Fit
#'
#' @param optim_control List passed on to \code{stats::optim}'s \code{control}
#'   argument. If not otherwise specified, the following arguments are set to
#'   non-default values: \code{fnscale = -1} and \code{lmm = 20}.
#'
#' @param maxit_conditional_modes Maximum number of iterations in penalized
#'   iteratively reweighted least squares algorithm. Ignored if \code{family =
#'   "gaussian"} for all observations, since then a single step gives the exact answer.
#'
#' @return Object of class galamm_control
#' @export
#'
galamm_control <- function(optim_control = list(), maxit_conditional_modes = 10) {
  if (length(trace) != 1L && trace < 0) {
    stop("trace should be a non-negative integer of length one")
  }

  stopifnot(length(maxit_conditional_modes) == 1 && maxit_conditional_modes > 0)

  if ("fnscale" %in% names(optim_control)) {
    if (optim_control$fnscale > 0) {
      stop("fnscale parameter should be negative. Otherwise loglikelihood will be minimized.")
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
    maxit_conditional_modes = maxit_conditional_modes
  )
}

new_galamm_control <- function(optim_control, maxit_conditional_modes) {
  if (is.null(optim_control$fnscale)) {
    optim_control$fnscale <- -1
  }

  if (is.null(optim_control$lmm)) {
    optim_control$lmm <- 20
  }


  ret <- list(
    maxit_conditional_modes = maxit_conditional_modes,
    optim_control = optim_control
  )

  class(ret) <- "galamm_control"
  ret
}



optim_control <- function(x) {
  UseMethod("optim_control")
}

#' Extract optim control parameters
#'
#' @param x Object of class galamm_control
#'
#' @return List
#' @aliases optim_control optim_control.galamm_control
#' @export optim_control
#' @export
#'
optim_control.galamm_control <- function(x) {
  x$optim_control
}
