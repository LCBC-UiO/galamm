#' Estimate a generalized additive latent and mixed model.
#'
#' @param formula The model formula, including smooth terms but not
#' random effects.
#' @param data Data frame or list.
#' @param family Family.
#' @param nAGQ Quadrature points.
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

  # Initial values, convert theta to log scale
  t0 <- glmod$reTrms$theta
  t0[t0 == 0] <- -10
  t0[t0 == 1] <- 0
  pars <- c(rep(0, ncol(glmod$X)), t0)

  hq <- hermite_quadrature
  Quadpoints <- hq[[nAGQ]]

  moments <- initialize_moments(glmod)


  nmom <- length(levels(glmod$reTrms$flist[[1]]))


  for(i in seq_along(glmod$reTrm$cnms[[1]])){
    moments$a <- rep(1, 10)
  }

  moments <- data.frame(mu1 = rep(0, nmom), tau1 = rep(1, nmom))

  Lambdat <- glmod$reTrms$Lambdat
  Lind <- glmod$reTrms$Lind
  X <- glmod$X
  Zt <- glmod$reTrms$Zt
  y <- glmod$fr$y
  Ztlist <- glmod$reTrms$Ztlist$`1 | id`




  # Run to get moments
  for(i in seq_len(maxit_newton)){
    ll <- eval_loglik(pars, moments, Quadpoints, Lambdat, Lind,
                      X, Zt, y, Ztlist, maxit = maxit_moments)
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


eval_loglik <- function(pars, moments, Quadpoints, Lambdat,
                        Lind, X, Zt, y, Ztlist, maxit = 1){
  fixed_inds <- seq_len(ncol(X))
  beta <- pars[fixed_inds]
  theta <- pars[setdiff(seq_along(pars), fixed_inds)]
  # Update relative covariance factor after having updated theta
  Lambdat@x[] <- exp(theta[Lind])

  ll0 <- -Inf
  for(i in seq_len(maxit)){
    alpha <- outer(moments$mu1, rep(1, length(Quadpoints$x))) +
      outer(moments$tau1, Quadpoints$x)

    pi <- sqrt(2 * pi) *
      outer(moments$tau1, exp(Quadpoints$x^2 / 2) * Quadpoints$w) *
      stats::dnorm(alpha)

    # Every column of alpha is a spherical random effect where we
    # want to evaluate the response probability
    # Create non-spherical random effects
    b <- Matrix::crossprod(Lambdat, alpha)

    # Evaluate linear predictor at each quadrature point
    nu <- Matrix::crossprod(Zt, b) + c(X %*% beta)

    # Evaluate response probability at each quadrature point
    probmat <- log_response_prob(linkinv(nu), y)

    # Sum of level-1 log probabilities per level-2 unit
    f1_prod <- Ztlist %*% probmat

    # Integrate to get level-2 likelihood
    f2 <- log(Matrix::rowSums(exp(f1_prod) * pi))
    moments$mu1 <- Matrix::rowSums(exp(f1_prod) * pi * alpha) / exp(f2)
    moments$tau1 <- sqrt(Matrix::rowSums(exp(f1_prod) * pi * alpha^2) / exp(f2) - moments$mu1^2)

    ll <- sum(f2)

    if(abs(ll0 - ll) < 1e-3) break
    ll0 <- ll
  }
  attr(ll, "moments") <- moments
  attr(ll, "iter") <- i
  ll
}
