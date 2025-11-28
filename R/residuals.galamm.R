#' @title Residuals of galamm objects
#'
#' @description Computes residuals for models fit with [galamm()] using the definitions in
#' Chapter 8 of \insertCite{dunnGeneralizedLinearModels2018;textual}{galamm}.
#' Define \eqn{y} as the response and \eqn{\hat{\mu}} as the model fit. Importantly,
#' \eqn{\hat{\mu}} includes all random effects. Also define \eqn{V(\cdot)} as the
#' variance function of the model family, and \eqn{w} as the weight. The Pearson
#' residual is then
#' \deqn{r_{P} = (y - \hat{\mu})/\sqrt{V(\hat{\mu}) / w}.}
#' Furthermore, let \eqn{sgn(\cdot)} be the function which returns the sign of its
#' argument and let \eqn{d(y, \hat{\mu})} be the model deviance. The deviance
#' residual is then
#' \deqn{r_{D} = sgn(y - \hat{\mu}) \sqrt{w d(y, \hat{\mu})}.}
#'
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.3,G2.3a} match.arg() used on "type" argument.
#' @srrstats {G2.3,G2.3b} Argument type is case sensitive, as stated in their
#'   documentation.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#' @srrstats {RE4.10} Model Residuals, including sufficient documentation to
#'   enable interpretation of residuals, and to enable users to submit residuals
#'   to their own tests.
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param type Character of length one describing the type of residuals to be
#'   returned. One of \code{"pearson"} and \code{"deviance"}. Argument is case
#'   sensitive.
#' @param scaled Logical value specifying whether to scale the residuals by
#'   their standard deviation. Defaults to \code{FALSE}.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#' @return Numeric vector of residual values.
#' @export
#'
#' @references \insertAllCited{}
#'
#' @seealso [fitted.galamm()] for model fitted values, [predict.galamm()] for
#'   model predictions, and [plot.galamm()] for diagnostic plots. The generic
#'   function is [residuals()].
#'
#' @family details of model fit
#'
#' @examples
#' # Poisson GLMM
#' count_mod <- galamm(
#'   formula = y ~ lbas * treat + lage + v4 + (1 | subj),
#'   data = epilep, family = poisson
#' )
#'
#' # Extract residuals
#' residuals(count_mod)
#'
residuals.galamm <- function(object, type = c("pearson", "deviance"),
                             scaled = FALSE, ...) {
  type <- match.arg(type, c("pearson", "deviance"))
  if (type == "pearson") {
    ret <- object$model$pearson_residuals
  } else if (type == "deviance") {
    ret <- object$model$deviance_residuals
  }

  if (scaled) {
    ret <- ret / stats::sd(ret)
  }
  ret
}
