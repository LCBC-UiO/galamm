#' @title Extract formula from fitted galamm object
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {RE4.4} The specification of the model, generally as a formula (via formula())
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
#' families <- gfam(list(gaussian, binomial))
#'
#'
#' # Fit the model
#' mod <- galamm(
#'   formula = y ~ x + (0 + level | id),
#'   data = mresp,
#'   family = families,
#'   factor = "level",
#'   load_var = "itemgroup",
#'   lambda = loading_matrix
#' )
#'
#' # Formula
#' formula(mod)
#'
formula.galamm <- function(x, ...) {
  x$call$formula
}
