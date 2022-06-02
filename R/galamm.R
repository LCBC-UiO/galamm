#' Fitting Generalized Additive Latent and Mixed Models
#'
#' @param formula a two-sided formula object using \code{lme4} style to specify
#' random effects.
#' @param data data frame containing the variables in \code{formula}.
#' @param family a GLM family, see \code{\link{glm}} and
#' \code{\link{family}}.
#' @param latent Formula for latent variables, of the form
#' \code{~(factor | load_var)}.
#' @param lambda List with initial matrices for factor loadings.
#'
#' @return Object of class \code{galamm}.
#' @export
#'
#' @examples
#' # empty example
galamm <- function(formula, data, family = gaussian, latent = NULL,
                  lambda = NULL){

  latent_barlist <- lme4::findbars(latent)
  factors <- find_factors(latent_barlist, data)
  load_vars <- find_load_vars(latent_barlist, data, lambda)
  lambda_init <- initialize_lambda(lambda)
  datax <- add_latent_to_data(latent_barlist, factors, load_vars, data,
                              lambda_init)
  X <- model.matrix(lme4::nobars(formula), data = datax)

  fixed_mapping <- find_load_cols(factors, load_vars, colnames(X))
  lambda_mapping <- lapply(names(lambda), function(x) as.integer(datax[[x]]))
  ranef_obj <- lme4::mkReTrms(lme4::findbars(formula), datax)

  ranef_mapping <- lapply(ranef_obj$cnms,
                          function(x) find_load_cols(factors, load_vars, x))

  y <- as.numeric(data[[all.vars(formula)[[1]]]])

  fit <- compute_galamm(
    y = y, X = X, Z = t(ranef_obj$Zt), Lambda = t(ranef_obj$Lambdat),
    Lind = ranef_obj$Lind - 1L, theta = ranef_obj$theta,
    theta_inds = seq(0L, length(ranef_obj$theta) - 1L),
    beta = rep(0, ncol(X)),
    beta_inds = length(ranef_obj$theta) + seq_len(ncol(X)) - 1L,
    phi = 950)
}
