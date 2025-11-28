##' @importFrom lattice qqmath
##' @export qqmath
NULL

#' @title Quantile-quantile plots for galamm objects
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#'
#' @param x An object of class \code{galamm}, returned from
#'   \code{\link{galamm}}.
#' @param data Ignored. Required for S3 method compatibility.
#' @param ... Optional parameters passed on to other methods. Currently not
#'   used.
#'
#' @return A quantile-quantile plot.
#'
#' @aliases qqmath qqmath.galamm
#'
#' @export
#'
#' @family diagnostics
#'
#' @author This function is derived from \code{lme4:::qqmath.merMod}, written by
#'   Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker.
#'
#' @examples
#' ## Linear mixed model example from lme4
#' data("sleepstudy", package = "lme4")
#' mod <- galamm(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' qqmath(mod)
#'
qqmath.galamm <- function(x, data = NULL, ...) {
  values <- residuals(x, type = "pearson", scaled = TRUE)

  qqpanel <- function(x, subscripts, ...) {
    dots <- list(...)
    lattice::panel.qqmathline(x, ...)
    lattice::panel.qqmath(x, ...)
  }
  lattice::qqmath(
    values, xlab = "Standard normal quantiles",
    ylab = "Standardized residuals",
    prepanel = lattice::prepanel.qqmathline,
    panel = qqpanel, ...)
}
