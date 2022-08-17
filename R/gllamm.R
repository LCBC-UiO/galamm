#' Fit the mixed model representation of a generalized
#' additive latent and mixed model
#'
#' Experimental function which computes maximum
#' likelihood estimates of the mixed model
#' representation of a GALAMM. The function can also
#' be used for fitting GLLAMMs.
#'
#' @param formula A formula for the fixed effects, including
#'   smooth terms.
#' @param random A formula for random effects.
#' @param family A GLM family.
#' @param data A data frame containing the variables used in the
#'   model.
#' @param load_var The variables which the factors load onto.
#' @param lambda A factor loading matrix, with row names corresponding
#'  to \code{load_var}.
#' @param factor The latent variable, or factor.
#'
#' @return An object of S3 class \code{galamm}.
#' @export
#'
#'
gllamm <- function(formula, family = stats::gaussian(),
                   data = list(), load_var = NULL, lambda = NULL,
                   factor = NULL){

  return(NULL)

}
