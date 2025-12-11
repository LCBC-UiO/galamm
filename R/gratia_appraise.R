##' @importFrom gratia appraise
##' @export appraise
NULL

#' Gratia style model diagnostic plots
#'
#' This function uses \code{gratia::appraise} to make model diagnostic plots.
#' See [gratia::appraise()] for details. When \code{object} is not of class
#' \code{galamm}, it is forwarded to \code{gratia::appraise()}.
#'
#' @return A ggplot object.
#' @export
#' @method appraise galamm
#' @rdname appraise
#' @family details of model fit
#' @examples
#' dat <- subset(cognition, domain == 1 & item == "11")
#' dat$y <- dat$y[, 1]
#' mod <- galamm(y ~ s(x) + (1 | id), data = dat)
#' appraise(mod)
#'
appraise.galamm <- function(object, ...) {
  if (is.null(object$gam)) {
    stop("No GAM object")
  }
  gratia::appraise(object$gam, ...)
}
