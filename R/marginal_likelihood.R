#' Compute the marginal likelihood of a GLLAMM or GALAMM
#'
#' This function computes the Laplace approximate marginal
#' likelihood.
#'
#' @param y A \code{numeric} vector of responses.
#' @param trials A \code{numeric} vector representing the number
#' of trials. Only used for binomial models, but should be set to
#' some arbitrary value otherwise.
#' @param X A dense \code{numeric} matrix.
#' @param Zt A sparse matrix of class \code{dgCMatrix}.
#' @param Lambdat A sparse matrix of class \code{dgCMatrix}.
#' @param beta A \code{numeric} vector of fixed effects.
#' @param theta A \code{numeric} vector of variance components,
#' parametrized identically to \code{lme4}.
#' @param theta_mapping An \code{integer} vector corresponding to
#' \code{Lind} used by \code{lme4}, but with base zero indexing.
#' @param u_init Initial values from random effects. Defaults to a vector of
#' zeros.
#' @param lambda A \code{numeric} vector of factor loadings.
#' @param lambda_mapping_X An \code{integer} vector of mappings between
#' \code{X} and \code{lambda}, columnwise. Should be set to
#' \code{integer()} if not used. An entry \code{-1} indicates that the
#' corresponding value of \code{X} does not depend on \code{lambda},
#' as in the case where the first element of \code{lambda} is fixed to 1.
#' @param lambda_mapping_Zt An \code{integer} vector of mappings between
#' \code{Zt} and \code{lambda}, along the nonzero elements of \code{Zt}
#' as can be found by \code{Zt@x}. Should be set to
#' \code{integer()} if not used. An entry \code{-1} indicates that the
#' corresponding value of \code{X} does not depend on \code{lambda},
#' as in the case where the first element of \code{lambda} is fixed to 1.
#' @param weights Vector of weights.
#' @param weights_mapping Mapping
#' @param family A length one \code{character} denoting the family.
#' @param family_mapping Mapping
#' @param maxit_conditional_modes Maximum number of iterations for
#' conditional models. Can be 1 when \code{family = "gaussian"}.
#' @param hessian Boolean specifying whether to include the Hessian matrix
#' at the given parameters. Defaults to \code{FALSE}.
#' @param epsilon_u Tolerance in the inner iteration. Defaults to \code{1e-10}.
#'
#' @return A \code{list} with elements \code{logLik} and \code{gradient}.
#' @export
#'
#' @details For examples, see the vignette on maximum likelihood estimation.
marginal_likelihood <- function(
    y, trials = rep(1, length(y)), X, Zt, Lambdat, beta, theta, theta_mapping,
    u_init = rep(0, nrow(Zt)),
    lambda = numeric(), lambda_mapping_X = integer(),
    lambda_mapping_Zt = integer(), weights = numeric(),
    weights_mapping = integer(), family = "gaussian",
    family_mapping = rep(0L, length(y)),
    maxit_conditional_modes = 1L, hessian = FALSE, epsilon_u = 1e-10){

  stopifnot(length(y) == length(trials))
  stopifnot(length(y) == nrow(X))
  stopifnot(ncol(Zt) == length(y))
  stopifnot(length(beta) == ncol(X))
  stopifnot(length(y) == length(family_mapping))
  stopifnot(length(family) == length(unique(family_mapping)))
  stopifnot(length(unique(family)) == length(family))

  k <- numeric(length(family))
  for(i in seq_along(k)){
    if(family[[i]] == "gaussian"){
      k[[i]] <- 0
    } else if(family[[i]] == "binomial"){
      trials0 <- trials[family_mapping == i - 1L]
      y0 <- y[family_mapping == i - 1L]
      k[[i]] <- sum(lgamma(trials0 + 1) - lgamma(y0 + 1) - lgamma(trials0 - y0 + 1))
    } else if(family[[i]] == "poisson"){
      trials0 <- trials[family_mapping == i - 1L]
      y0 <- y[family_mapping == i - 1L]
      k[[i]] <- -sum(lgamma(y0 + 1))
    }
  }

  marginal_likelihood_cpp(
    y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, u_init, lambda,
    lambda_mapping_X, lambda_mapping_Zt, weights, weights_mapping,
    family, family_mapping, k, maxit_conditional_modes, hessian, epsilon_u
  )



}

