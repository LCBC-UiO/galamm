#' @title Diagnostic plots for galamm objects
#'
#' @description This function provides diagnostic plots for models fitted with
#'   [galamm()]. See the [residuals.galamm()] function for definition of the
#'   residuals being used.
#'
#' @param x An object of class \code{galamm} returned from \code{\link{galamm}}.
#' @param form An option formula specifying the desired type of plot.
#' @param ... Optional arguments passed on to the \code{plot} function.
#' @return A plot is displayed.
#' @export
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#' @srrstats {RE6.0,RE6.2} Default plot method.
#' @srrstats {RE6.1,RE6.3} Not applicable.
#'
#' @seealso [residuals.galamm()] for extracting residuals and [plot()] for the
#'   generic function.
#'
#' @family summary functions
#' @author Douglas Bates, Martin Maechler, Ben Bolker, and Steven Walker, with
#' modifications by Øystein Sørensen.
#'
#' @details The interface of this function is designed to be similar to the
#'   `plot.merMod` function from `lme4`
#'   \insertCite{batesFittingLinearMixedEffects2015}{galamm}.
#'
#' @references \insertAllCited{}
#'
#' @examples
#' # Linear mixed model example from lme4
#' data("sleepstudy", package = "lme4")
#' mod <- galamm(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'
#' # Diagnostic plot
#' plot(mod)
#'
#' # Logistic mixed model example from lme4
#' data("cbpp", package = "lme4")
#' mod <- galamm(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'               data = cbpp, family = binomial)
#'
#' # Diagnostic plot using Pearson residuals
#' plot(mod)
#'
#' # Diagnostic plot using deviance residuals
#' plot(mod, resid(., type = "deviance") ~ fitted(.))
#'
plot.galamm <- function(x, form = resid(., type = "pearson") ~ fitted(.), ...) {
  object <- x
  allV <- all.vars(nlme::asOneFormula(form))
  args <- list()

  if (length(allV > 0)) {

  } else {
    data <- NULL
  }

  data <- as.list(c(as.list(data), . = list(object)))
  covF <- nlme::getCovariateFormula(form)
  .x <- eval(covF[[2]], data)

  argForm <- ~ .x
  argData <- data.frame(.x = .x, check.names = FALSE)
  respF <- nlme::getResponseFormula(form)
  if (!is.null(respF)) {
    .y <- eval(respF[[2]], data)
    argForm <- .y ~ .x
    argData[, ".y"] <- .y
  }

  args <- c(list(argForm, data = argData), args)

  do.call("xyplot", as.list(args))
}
