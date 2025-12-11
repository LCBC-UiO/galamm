##' @importFrom gratia appraise
##' @export appraise
NULL

#' Gratia style model diagnostic plots
#'
#' This function uses \code{gratia::appraise} to make model diagnostic plots.
#' See [gratia::appraise()] for details. When \code{model} is not of class
#' \code{galamm}, it is forwarded to \code{gratia::appraise()}.
#'
#' @param model An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param ... Other arguments passed on to [gratia::appraise()].
#' @return A ggplot object.
#' @export
#' @aliases appraise appraise.galamm
#' @family details of model fit
#' @examples
#' dat <- subset(cognition, domain == 1 & item == "11")
#' dat$y <- dat$y[, 1]
#' mod <- galamm(y ~ s(x) + (1 | id), data = dat)
#' appraise(mod)
#'
appraise.galamm <- function(model, ...) {
  if (is.null(model$gam) || length(model$gam) == 0) {
    stop("No GAM object")
  }
  gratia::appraise(model$gam, ...)
}
