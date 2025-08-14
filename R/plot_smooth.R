plot_smooth <- function(object, ...) {
  UseMethod("plot_smooth")
}

#' @title Plot smooth terms for galamm fits
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#'
#' @description
#' Plots smooth terms of a fitted \code{galamm} object. This function is a thin
#' wrapper around \code{mgcv::plot.gam}
#' \insertCite{woodGeneralizedAdditiveModels2017}{galamm}.
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
#' @family summary functions
#'
#' @references \insertAllCited{}
#'
#' @examples
#' # Generalized additive mixed model with factor structures -------------------
#'
#' # The cognition dataset contains simulated measurements of three latent
#' # time-dependent processes, corresponding to individuals' abilities in
#' # cognitive domains. We focus here on the first domain, and take a single
#' # random timepoint per person:
#' dat <- subset(cognition, domain == 1)
#' dat <- split(dat, f = dat$id)
#' dat <- lapply(dat, function(x) x[x$timepoint %in% sample(x$timepoint, 1), ])
#' dat <- do.call(rbind, dat)
#' dat$item <- factor(dat$item)
#'
#' # At each timepoint there are three items measuring ability in the cognitive
#' # domain. We fix the factor loading for the first measurement to one, and
#' # estimate the remaining two. This is specified in the loading matrix.
#' loading_matrix <- matrix(c(1, NA, NA), ncol = 1)
#'
#' # We can now estimate the model.
#' mod <- galamm(
#'   formula = y ~ 0 + item + sl(x, factor = "loading") +
#'     (0 + loading | id),
#'   data = dat,
#'   load_var = "item",
#'   lambda = loading_matrix,
#'   factor = "loading"
#' )
#'
#' # We can plot the estimated smooth term
#' plot_smooth(mod, shade = TRUE)
#'
#' # We can turn off the rug at the bottom
#' plot_smooth(mod, shade = TRUE, rug = FALSE)
#'
plot_smooth.galamm <- function(object, ...) {
  if (!exists("gam", object) || length(object$gam) == 0) {
    stop("No terms to plot.")
  }

  plot(object$gam, ...)
}
