#' Class "galamm"
#'
#' An S3 class for objects returned by [galamm()].
#' Contains information about the fitted model, random effects, parameters, and
#' smooth terms.
#'
#' @section Components:
#' * `call`: the matched call used when fitting the model.
#' * `random_effects`: a list with:
#'   * `b`: random effects in original parametrization.
#'   * `u`: random effects standardized to have identity covariance matrix.
#' * `model`: a list containing:
#'   * `deviance`: deviance of final model.
#'   * `deviance_residuals`: deviance residuals.
#'   * `df`: degrees of freedom.
#'   * `family`: one or more family objects.
#'   * `factor_interactions`: list of formulas for latent-observed interactions.
#'   * `fit`: fitted values.
#'   * `fit_population`: fitted values excluding random effects.
#'   * `hessian`: Hessian matrix of the final model.
#'   * `lmod`: linear model object from [lme4::lFormula()].
#'   * `loglik`: log-likelihood of the final model.
#'   * `n`: number of observations.
#'   * `pearson_residual`: Pearson residuals.
#'   * `reduced_hessian`: logical; whether full Hessian or reduced version.
#'   * `response`: numeric vector of response values.
#'   * `weights_object`: object with weights (or `NULL`).
#' * `parameters`: a list with model parameters and metadata:
#'   * `beta_inds`: indices of fixed regression coefficients.
#'   * `dispersion_parameter`: dispersion parameters.
#'   * `lambda_dummy`: dummy factor loading matrix.
#'   * `lambda_inds`: indices of factor loadings.
#'   * `lambda_interaction_inds`: indices of latent-observed interactions.
#'   * `parameter_estimates`: numeric vector of parameter estimates.
#'   * `parameter_names`: names of all parameters.
#'   * `theta_inds`: indices of variance components (Cholesky entries).
#'   * `weights_inds`: indices of estimated weights.
#' * `gam`: list of smooth term info (empty list if none).
#'
#' @docType class
#' @name galammObject
NULL
