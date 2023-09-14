##' @importFrom nlme VarCorr
##' @export VarCorr
NULL

#' Extract variance and correlation components from model
#'
#' @param x An object of class \code{galamm} returned from \code{\link{galamm}}.
#' @param sigma Numeric value used to multiple the standard deviations. Defaults
#'   to 1.
#' @param ... Other arguments passed onto other methods. Currently not used.
#'
#' @name VarCorr
#' @aliases VarCorr VarCorr.galamm
#'
#' @return An object of class \code{VarCorr.galamm}.
#' @export
#'
#' @seealso [print.VarCorr.galamm()] for the print function.
#'
VarCorr.galamm <- function(x, sigma = 1, ...) {
  useSc <- Reduce(function(`&&`, y) y$family == "gaussian",
    x$family,
    init = TRUE
  )

  structure(
    lme4::mkVarCorr(sqrt(x$phi)[[1]], x$cnms,
      nc = lengths(x$cnms),
      theta = x$par[x$theta_inds], names(x$cnms)
    ),
    useSc = useSc,
    class = "VarCorr.galamm"
  )
}


#' Print method for variance-covariance objects
#'
#'
#' @param x An object of class \code{VarCorr.galamm}, returned from
#'   \code{\link{VarCorr.galamm}}.
#' @param digits Optional arguments specifying number of digits to use when
#'   printing.
#' @param comp Character vector of length 1 or 2 specifying which variance
#'   components to print.
#' @param corr Boolean indicating whether covariances or correlations should be
#'   printed.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#' @return The variance-covariance information is printed to the console and the
#'   argument \code{x} is silently returned.
#' @export
#'
#' @author This function is derived from \code{lme4:::print.VarCorr.merMod}
#'   written by Douglas M. Bates, Martin Maechler, Ben Bolker, and Steve Walker.
#'
#' @references \insertRef{batesFittingLinearMixedEffects2015}{galamm}
#'
#' @seealso [VarCorr.galamm()] for the function creating the variance-covariance
#'   objects.
#'
print.VarCorr.galamm <- function(x, digits = max(3, getOption("digits") - 2),
                                 comp = c("Std.Dev.", "Variance"), corr = any(comp == "Std.Dev."), ...) {
  comp <- match.arg(comp, several.ok = TRUE)
  print(lme4::formatVC(x, digits = digits, comp = comp, corr = corr),
    quote = FALSE, ...
  )
  invisible(x)
}
