#' Fit a generalized additive latent and mixed model
#'
#' Fits a GALAMM
#'
#'
#' @param formula A formula for the fixed effects, including
#'   smooth terms.
#' @param random A formula for random effects.
#' @param family A GLM family.
#' @param data A data frame containing the variables used in the
#'   model.
#' @param load.var The variables which the factors load onto.
#' @param lambda A factor loading matrix, with row names corresponding
#'  to \code{load.var}.
#' @param factor The latent variable, or factor.
#'
#' @return An object of S3 class \code{galamm}.
#' @export
#'
#'
#'
galamm <- function(formula, random = NULL, family = stats::gaussian(),
                   data = list(), load.var = NULL, lambda = NULL,
                   factor = NULL){

  res <- 0
  class(res) <- "galamm"

  res

}
