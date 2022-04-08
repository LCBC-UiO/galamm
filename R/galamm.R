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

  barlist <- lme4::findbars(latent)
  factors <- lapply(barlist, function(x) as.character(x)[[2]])

  if(any(factors %in% names(data))){
    stop("Factors in argument 'latent' cannot be columns of 'data'.")
  }

  load_vars <- lapply(barlist, function(x) as.character(x)[[3]])
  if(!all(load_vars %in% names(data))){
    stop("All loading variables in 'latent' must be columns of 'data'.")
  }


  lme4::mkReTrms(bars = barlist, fr = data)

}
