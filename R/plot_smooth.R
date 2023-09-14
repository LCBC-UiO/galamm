plot_smooth <- function(object, ...) {
  UseMethod("plot_smooth")
}

#' Plot smooth terms for galamm fits
#'
#' Plots smooth terms of a fitted \code{galamm} object. This function is a thin
#' wrapper around \code{mgcv::plot.gam}
#' \insertCite{woodGeneralizedAdditiveModels2017a}{galamm}.
#'
#' @aliases plot_smooth plot_smooth.galamm
#' @export plot_smooth
#' @export
#'
#' @param object Object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param ... Other optional arguments, passed on to \code{mgcv::plot.gam}.
#'
#' @return A plot is displayed on the screen.
#' @export
#'
#' @references \insertAllCited{}
#'
plot_smooth.galamm <- function(object, ...) {
  if (!exists("gam", object)) stop("No terms to plot.")

  plot(object$gam, ...)
}
