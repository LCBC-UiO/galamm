#' Estimate a generalized additive latent and mixed model.
#'
#' @param formula The model formula, including smooth terms but not
#' random effects.
#' @param data Data frame or list.
#' @param family Family.
#' @param nAGQ Quadrature points. Either a vector of the same length as the
#' number of random effects, or a single value.
#' @param maxit_newton Newton iterations.
#' @param maxit_moments Moment iterations.
#'
#' @return An object of class galamm.
#' @export
#'
#' @example /inst/examples/galamm_examples.R
#'
galamm <- function(formula, data, family, nAGQ,
                   maxit_newton = 10, maxit_moments = 10) {

  glmod <- lme4::glFormula(formula = formula,
                           data = data,
                           family = family)

  devfun <- do.call(lme4::mkGlmerDevfun, glmod)
  rho <- environment(devfun)

  moments <- initialize_moments(glmod)

  hq <- hermite_quadrature
  Quadpoints <- lapply(nAGQ, function(x) hq[[x]])

  pars <- c(rho$pp$beta0, rho$pp$theta)
  fixed_ind <- seq_along(rho$pp$beta0)
  vc_ind <- seq_along(rho$pp$theta) + max(fixed_ind)

  LLHelper <- Matrix::sparseMatrix(
    i = as.integer(as.character(glmod$reTrms$flist$id)),
    j = seq(from = 1, to = length(glmod$reTrms$flist$id)),
    x = 1
  )

  # Run to get moments
  for(i in seq_len(maxit_newton)){
    ll <- eval_loglik(pars, moments, Quadpoints,
                      LLHelper = LLHelper,
                      Lambdat = rho$pp$Lambdat,
                      Lind = rho$pp$Lind,
                      X = rho$pp$X,
                      Zt = rho$pp$Zt,
                      y = rho$resp$y,
                      family = rho$resp$family,
                      maxit = maxit_moments,
                      fixed_ind, vc_ind)
    moments <- attr(ll, "moments")
    g <- numDeriv::grad(eval_loglik, pars,
                        moments = moments,
                        Quadpoints = Quadpoints,
                        Lambdat = Lambdat,
                        Lind = Lind,
                        X = X, Zt = Zt, y = y, Ztlist = Ztlist,
                        maxit = 1)
    H <- numDeriv::hessian(eval_loglik, pars,
                           moments = moments,
                           Quadpoints = Quadpoints,
                           Lambdat = Lambdat,
                           Lind = Lind,
                           X = X, Zt = Zt, y = y, Ztlist = Ztlist,
                           maxit = 1)

    pars <- pars - solve(H) %*% g
  }


  pars


}


