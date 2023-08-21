#' Diagnostic Plots for galamm Objects
#'
#' @param x A galamm object
#' @param ... Other arguments
#'
#' @return A plot is displayed
#' @export
#'
#' @importFrom graphics abline
#' @importFrom stats fitted residuals
plot.galamm <- function(x, ...) {
  plot(fitted(x), residuals(x, type = "pearson"),
    xlab = "Predicted values",
    ylab = "Pearson residuals",
    ...
  )
  abline(h = 0, col = "blue")
}
