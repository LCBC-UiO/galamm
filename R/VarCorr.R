##' @importFrom nlme VarCorr
##' @export VarCorr
NULL

#' @title Extract variance and correlation components from model
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#'
#' @param x An object of class \code{galamm} returned from \code{\link{galamm}}.
#' @param sigma Numeric value used to multiply the standard deviations. Defaults
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
#' @family details of model fit
#'
#' @examples
#' # Linear mixed model with heteroscedastic residuals
#' mod <- galamm(
#'   formula = y ~ x + (1 | id),
#'   weights = ~ (1 | item),
#'   data = hsced
#' )
#'
#' # Extract information on variance and covariance
#' VarCorr(mod)
#'
VarCorr.galamm <- function(x, sigma = 1, ...) {
  useSc <- Reduce(function(`&&`, y) y$family == "gaussian",
    family(x),
    init = TRUE
  )

  structure(
    lme4::mkVarCorr(sigma(x)[[1]], x$model$lmod$reTrms$cnms,
      nc = lengths(x$model$lmod$reTrms$cnms),
      theta = x$parameters$parameter_estimates[x$parameters$theta_inds],
      names(x$model$lmod$reTrms$cnms)
    ),
    useSc = useSc,
    class = "VarCorr.galamm"
  )
}


#' @title Print method for variance-covariance objects
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#' @srrstats {G2.3a} match.arg() used on "comp" argument.
#' @srrstats {G2.3b} Argument "comp" is case sensitive, as is documented here.
#'
#' @param x An object of class \code{VarCorr.galamm}, returned from
#'   \code{\link{VarCorr.galamm}}.
#' @param digits Optional arguments specifying number of digits to use when
#'   printing.
#' @param comp Character vector of length 1 or 2 specifying which variance
#'   components to print. Case sensitive. Can take one of the values "Std.Dev."
#'   and "Variance".
#' @param corr Logical value indicating whether covariances or correlations
#'   should be printed.
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
#' @family details of model fit
#'
#' @examples
#' # Linear mixed model with heteroscedastic residuals
#' mod <- galamm(
#'   formula = y ~ x + (1 | id),
#'   weights = ~ (1 | item),
#'   data = hsced
#' )
#'
#' # Extract information on variance and covariance
#' VarCorr(mod)
#'
print.VarCorr.galamm <- function(x, digits = max(3, getOption("digits") - 2),
                                 comp = c("Std.Dev.", "Variance"),
                                 corr = any(comp == "Std.Dev."), ...) {
  comp <- match.arg(comp, several.ok = TRUE)
  print(lme4::formatVC(x, digits = digits, comp = comp, corr = corr),
    quote = FALSE, ...
  )
  invisible(x)
}
