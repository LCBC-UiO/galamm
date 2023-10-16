#' Extract formula from fitted galamm object
#'
#' @param x Object of class \code{galamm} returned from \code{\link{galamm}}.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#' @return The formula used to fit the model.
#' @export
#'
#' @family details of model fit
#'
#' @examples
#' # Mixed response model ------------------------------------------------------
#'
#' # The mresp dataset contains a mix of binomial and Gaussian responses.
#'
#' # We need to estimate a factor loading which scales the two response types.
#' loading_matrix <- matrix(c(1, NA), ncol = 1)
#'
#' # Define mapping to families.
#' families <- c(gaussian, binomial)
#' family_mapping <- ifelse(mresp$itemgroup == "a", 1, 2)
#'
#'
#' # Fit the model
#' mod <- galamm(
#'   formula = y ~ x + (0 + level | id),
#'   data = mresp,
#'   family = families,
#'   family_mapping = family_mapping,
#'   factor = list("level"),
#'   load.var = "itemgroup",
#'   lambda = list(loading_matrix)
#' )
#'
#' # Formula
#' formula(mod)
#'
formula.galamm <- function(x, ...) {
  x$call$formula
}
