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
#' @importFrom Rdpack reprompt
galamm <- function(formula, data, family) {
  print("Hello, world!")

  gaussquad::hermite.h.quadrature.rules(4)[[4]]
}
