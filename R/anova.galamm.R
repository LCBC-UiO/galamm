#' Compare Likelihoods of galamm Objects
#'
#' @param object A model fitted by \code{\link{galamm}}
#' @param ... Other optional fitted models of class \code{galamm}.
#'
#' @return A table
#' @export
#'
#' @importFrom stats AIC BIC deviance logLik
anova.galamm <- function(object, ...) {
  dots <- list(...)

  tab <- make_anova(object)

  for (mm in dots) {
    tab <- rbind(tab, make_anova(mm))
  }
  tab
}



make_anova <- function(object) {
  cbind(
    npar = object$df,
    AIC = AIC(object),
    BIC = BIC(object),
    logLik = as.numeric(logLik(object)),
    deviance = deviance(object)
  )
}
