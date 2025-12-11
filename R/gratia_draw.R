##' @importFrom gratia draw
##' @export draw
NULL

#' Draw method for galamm objects
#'
#' This function uses \code{gratia::draw} to visualize smooth terms. See
#' [gratia::draw()] for details. When \code{object} is not of class
#' \code{galamm}, it is forwarded to \code{gratia::draw()}.
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param ... Other arguments passed on to [gratia::draw()].
#' @return A ggplot object.
#' @export
#' @aliases draw draw.galamm
#' @family summary functions
#' @examples
#' dat <- subset(cognition, domain == 1 & item == "11")
#' dat$y <- dat$y[, 1]
#' mod <- galamm(y ~ s(x) + (1 | id), data = dat)
#'
#' draw(mod)
#'
draw.galamm <- function(object, ...) {
  if (is.null(object$gam) || length(object$gam) == 0) {
    stop("No GAM object")
  }
  gratia::draw(object$gam, ...)
}
