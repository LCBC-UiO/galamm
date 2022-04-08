#' Fitting Generalized Additive Latent and Mixed Models
#'
#' @param formula a two-sided formula object using \code{lme4} style to specify
#' random effects.
#' @param data data frame containing the variables in \code{formula}.
#' @param family a GLM family, see \code{\link{glm}} and
#' \code{\link{family}}.
#' @param latent Formula for latent variables, of the form
#' \code{~(factor | load_var)}.
#' @param lambda Initial matrix for factor loadings.
#'
#' @return Object of class \code{galamm}.
#' @export
#'
#' @examples
#' # empty example
galamm <- function(formula, data, family = gaussian,
                   latent = NULL, lambda = NULL){

  latent_barlist <- lme4::findbars(latent)
  factors <- find_factors(latent_barlist, data)
  load_vars <- find_load_vars(latent_barlist, data, lambda)
  lambda_init <- initialize_lambda(lambda)
  datax <- add_latent_to_data(latent_barlist, factors, load_vars, data,
                              lambda_init)
  X <- create_fixed_model_matrix(formula, datax, factors, load_vars)
  fixed_mappings <- attr(X, "fixed_mappings")


  ## Here is how we can update the values of X
  tmp <- update_fixed(X, fixed_mappings[[1]]$col_inds - 1L,
                      c(1, .5, 2)[fixed_mappings[[1]]$lambda_ind])

}
