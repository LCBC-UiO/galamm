#' @title Extract family or families from fitted galamm
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#' @description
#' This function returns a list of families for an object of class
#' \code{galamm}, returned from \code{\link{galamm}}.
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#' @return A list of family objects.
#' @export
#'
#' @seealso [galamm()]
#'
#' @family details of model fit
#'
#' @examples
#' # Mixed response model
#' loading_matrix <- matrix(c(1, NA), ncol = 1)
#' families <- c(gaussian, binomial)
#' family_mapping <- ifelse(mresp$itemgroup == "a", 1, 2)
#'
#' mixed_resp <- galamm(
#'   formula = y ~ x + (0 + level | id),
#'   data = mresp,
#'   family = families,
#'   family_mapping = family_mapping,
#'   load.var = "itemgroup",
#'   lambda = loading_matrix,
#'   factor = "level"
#' )
#'
#' # This model has two family objects
#' family(mixed_resp)
#'
family.galamm <- function(object, ...) {
  object$model$family
}
