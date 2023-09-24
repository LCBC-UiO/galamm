extract_optim_parameters <- function(object) {
  UseMethod("extract_optim_parameters")
}

#' Extract parameters from fitted model for use as initial values
#'
#' This function extracts parameter values from a fitted model object in a form
#' that can be directly provided as initial values for a new model fit.
#'
#' @aliases extract_optim_parameters extract_optim_parameters.galamm
#' @export extract_optim_parameters
#' @export
#'
#' @param object Object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#'
#' @return A \code{list} object containing the following elements:
#' * \code{theta} Numerical vector of variance components, i.e., entries of
#'   the lower Cholesky form of the covariance matrix of random effects.
#' * \code{beta} Fixed regression coefficients.
#' * \code{lambda} Factor loadings.
#' * \code{weights} Weights for heteroscedastic residuals.
#'
#'
#' @family modeling functions
#'
#' @examples
#' # Fit linear mixed model with heteroscedastic residuals
#' mod <- galamm(
#'   formula = y ~ x + (1 | id),
#'   weights = ~ (1 | item),
#'   data = hsced
#' )
#'
#' # Extract parameters
#' start <- extract_optim_parameters(mod)
#'
#' # Fit again using the Nelder-Mead algorithm, using start as initial values:
#' mod_nm <- galamm(
#'   formula = y ~ x + (1 | id),
#'   weights = ~ (1 | item),
#'   data = hsced,
#'   start = start,
#'   control = galamm_control(method = "Nelder-Mead")
#' )
#'
extract_optim_parameters.galamm <- function(object) {
  list(
    theta = object$parameters$parameter_estimates[
      object$parameters$theta_inds
    ],
    beta = object$parameters$parameter_estimates[
      object$parameters$beta_inds
    ],
    lambda = object$parameters$parameter_estimates[
      object$parameters$lambda_inds
    ],
    weights = object$parameters$parameter_estimates[
      object$parameters$weights_inds
    ]
  )
}
