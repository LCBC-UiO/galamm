#' GALAMM plotting
#'
#' This function plots fitted \code{\link{galamm}} objects. It uses \code{mgcv::plot.gam}, but
#' the confidence bands take uncertainty about factor loadings into account.
#'
#' @param x A fitted \code{galamm} object as produced by \code{\link{galamm}}.
#' @param ... Other arguments passed on to \code{mgcv::plot.gam}.
#'
#' @return A plot is generated.
#' @export
#'
#' @examples
#' # Example dataset with three measurements of a latent response
#' # Define loading matrix
#' load.mat <- matrix(c(1, NA, NA), ncol = 1)
#' dimnames(load.mat) <- list(c("item1", "item2", "item3"), NULL)
#' \dontrun{
#' mod <- galamm(
#'   formula = y ~ s(x, by = weight),
#'   random = ~(1|id),
#'   data = dat1,
#'   load_var = "item",
#'   lambda = load.mat,
#'   factor = "weight",
#'   lambda_init = c(2, .4),
#'   optim_control = list(trace = 3)
#'   )
#' plot(mod)
#'   }
#'
plot.galamm <- function(x, ...){
  x$gamm4$gam$Ve <- x$gamm4$gam$Vp <- x$cov_beta
  plot(x$gamm4$gam, ...)
}
