#' Confidence intervals for model parameters
#'
#' @param object Model object
#' @param parm Parameters for which to compute intervals. Use "theta" to get all
#'   variance parameters, "beta" to get all fixed regression coefficients,
#'   "lambda" to get all factor loadings, and "weights" to get all weights. Can
#'   also be a vector of number.
#' @param level Confidence level.
#' @param method Currently only "Wald" is available.
#' @param ...
#'
#' @return Confidence intervals.
#' @export
#'
confint.galamm <- function(object, parm, level = 0.95,
                           method = "Wald", ...) {
  method <- match.arg(method, "Wald")
  stopifnot(length(level) == 1 && level > 0 && level < 1)

  inds <- find_parm_inds(object, parm)

  cf <- object$par[inds]
  ses <- sqrt(diag(vcov(object, parm)))

  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  fac <- qnorm(a)
  pct <- paste(
    format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%"
  )
  ci <- array(NA_real_,
    dim = c(length(inds), 2L),
    dimnames = list(inds, pct)
  )
  ci[] <- cf + ses %o% fac
  ci
}
