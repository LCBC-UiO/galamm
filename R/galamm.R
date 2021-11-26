#' Estimate a generalized additive latent and mixed model.
#'
#' @param formula The model formula, including smooth terms but not
#' random effects.
#' @param data Data frame or list.
#' @param family Family.
#'
#' @return An object of class galamm.
#' @export
#'
#'
galamm <- function(formula, data, family, nAGQ) {

  glmod <- lme4::glFormula(formula = formula,
                           data = data,
                           family = family)

  family <- glmod$family

  # Fixed effect model matrix
  X <- glmod$X

  # Transposed sparse model matrix for random effects
  Zt <- glmod$reTrms$Zt

  # Initial values of covariance parameters
  theta <- glmod$reTrms$theta

  # Initial values of regression coefficients
  beta <- c(.2, .3)

  # Transposed relative covariance factor
  Lambdat <- glmod$reTrms$Lambdat

  # Quadrature points
  Quadpoints <- sweep(hermite.h.quadrature.rules(nAGQ)[[nAGQ]], 2,
                      c(sqrt(2), 1 / sqrt(pi)), "*")

  # Assuming the random effects are strictly hierarchical
  nmom <- length(levels(glmod$reTrms$flist[[1]]))
  moments <- data.frame(mu1 = rep(0, nmom), tau1 = rep(1, nmom))

  alpha <- outer(moments$mu1, rep(1, length(Quadpoints$x))) +
    outer(moments$tau1, Quadpoints$x)
  pi <- sqrt(2 * pi) *
    outer(moments$tau1, exp(Quadpoints$x^2 / 2) * Quadpoints$w) *
    dnorm(alpha)

  # Every column of alpha is a spherical random effect where we
  # want to evaluate the response probability
  # Create non-spherical random effects
  b <- crossprod(Lambdat, alpha)

  # Evaluate linear predictor at each quadrature point
  nu <- crossprod(Zt, b) + c(X %*% beta)

  # Response
  y <- glmod$fr$y
  trials <- 1


  linkinv <- function(x) (1 + exp(-x))^(-1)
  log_response_prob <- function(mean, y, trials){
    y * log(mean) + (trials - y) * log(1 - mean)
  }

  # Evaluate response probability at each quadrature point
  probmat <- log_response_prob(linkinv(nu), y, trials)

  # Mapping
  Ztlist <- glmod$reTrms$Ztlist$`1 | id`

  # Sum of level-1 log probabilities per level-2 unit
  f1_prod <- Ztlist %*% probmat

  # Integrate to get level-2 likelihood
  f2 <- rowSums(f1_prod * pi)
  moments$mu1 <- rowSums(f1_prod * pi * alpha) / f2
  moments$tau1 <- sqrt(rowSums(f1_prod * pi * alpha^2) / f2 - moments$mu1^2)




  # Update relative covariance factor after having updated theta
  Lambdat@x[] <- theta[glmod$reTrms$Lind]
}
