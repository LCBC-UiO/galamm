#' Diagnostic plots for galamm objects
#'
#' @param x An object of class \code{galamm} returned from \code{\link{galamm}}.
#' @param ... Optional arguments passed on to the \code{plot} function.
#'
#' @return A plot is displayed.
#' @export
#'
#' @seealso [residuals.galamm()] for extracting residuals and [plot()] for the
#'   generic function.
#'
#' @examples
#' # Linear mixed model with factor structures
#' library(PLmixed) # needed for KYPSsim dataset
#'
#' # Define loading matrix
#' loading_matrix <- list(rbind(c(1, 0), c(NA, 0), c(NA, 1), c(NA, NA)))
#'
#' # Model formula
#' form <- esteem ~ time + (0 + ms | mid) + (0 + hs | hid) + (1 | sid)
#'
#' # Estimate model
#' mod <- galamm(formula = form, data = KYPSsim, factor = list(c("ms", "hs")),
#'               load.var = "time", lambda = loading_matrix)
#'
#' # Diagnostic plot
#' plot(mod)
#'
plot.galamm <- function(x, ...) {
  plot(fitted(x), residuals(x, type = "pearson"),
    xlab = "Predicted values",
    ylab = "Pearson residuals",
    ...
  )
  graphics::abline(h = 0, col = "blue")
}
