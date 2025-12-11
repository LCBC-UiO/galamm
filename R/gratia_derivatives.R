##' @importFrom gratia derivatives
##' @export derivatives
NULL

#' Derivatives of estimated smooth via finite differences
#'
#' This function uses \code{gratia::derivatives} to compute derivatives of
#' estimated smooth via finite differences. See
#' [gratia::derivatives()] for details. When \code{object} is not of class
#' \code{galamm}, it is forwarded to \code{gratia::derivatives()}.
#'
#' @return A a tibble.
#' @export
#' @method derivatives galamm
#' @rdname derivatives
#' @family details of model fit
#' @examples
#' dat <- subset(cognition, domain == 1 & item == "11")
#' dat$y <- dat$y[, 1]
#' mod <- galamm(y ~ s(x) + (1 | id), data = dat)
#'
#' dd <- derivatives(mod)
#' draw(dd)
#'
derivatives.galamm <- function(object, ...) {
  if (is.null(object$gam)) {
    stop("No GAM object")
  }
  gratia::derivatives(object$gam, ...)
}
