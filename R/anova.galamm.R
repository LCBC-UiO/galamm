#' Compare Likelihoods of galamm Objects
#'
#' @param object A model fitted by \code{\link{galamm}}
#' @param ... Other optional fitted models of class \code{galamm}.
#'
#' @details
#' Some of the source code for this function is adapted from
#' \code{lme4:::anova.merMod}.
#'
#'
#' @return A table
#' @export
#'
#' @importFrom stats AIC BIC deviance logLik
anova.galamm <- function(object, ...) {
  mCall <- match.call(expand.dots = TRUE)
  dots <- list(...)
  tab <- make_anova(object)

  modp <- vapply(dots, is, TRUE, "galamm")


  mNms <- vapply(as.list(mCall)[c(FALSE, TRUE, modp)],
                 deparse1, "")

  if(any(modp)){
    mods <- c(list(object), dots[modp])
    nobs.vec <- vapply(mods, nobs, 1L)
    if (var(nobs.vec) > 0)
      stop("models were not all fitted to the same size of dataset")
    mNms <- vapply(as.list(mCall)[c(FALSE, TRUE, modp)], deparse1, "")
    names(mods) <- mNms
    llks <- lapply(mods, logLik)
    ii <- order(npar <- vapply(llks, attr, FUN.VALUE = numeric(1), "df"))
    mods <- mods[ii]
    llks <- llks[ii]
    npar <- npar[ii]

    calls <- lapply(mods, getCall)
    data <- lapply(calls, `[[`, "data")
    if (!all(vapply(data, identical, NA, data[[1]])))
      stop("all models must be fit to the same data object")
    header <- paste("Data:", abbrDeparse(data[[1]]))

    llk <- unlist(llks)
    chisq <- 2 * pmax(0, c(NA, diff(llk)))
    dfChisq <- c(NA, diff(npar))

    val <- data.frame(
      npar = npar,
      AIC = vapply(llks, AIC, 1),
      BIC = vapply(llks, BIC, 1),
      logLik = llk,
      deviance = -2 * llk, Chisq = chisq,
      Df = dfChisq,
      `Pr(>Chisq)` = ifelse(dfChisq == 0, NA,
                            pchisq(chisq, dfChisq, lower.tail = FALSE)),
      row.names = names(mods), check.names = FALSE)
    class(val) <- c("anova", class(val))
    forms <- lapply(lapply(calls, `[[`, "formula"), deparse1)

    structure(
      val,
      heading = c(header, "Models:",
                  paste(rep.int(names(mods), lengths(forms)), unlist(forms),
                        sep = ": ")))
  } else {
    tab
  }



}



make_anova <- function(object) {
  data.frame(
    npar = object$df,
    AIC = AIC(object),
    BIC = BIC(object),
    logLik = as.numeric(logLik(object)),
    deviance = deviance(object)
  )
}


#' Abbreviated deparse function taken from lme4
#'
#' @param x Name to deparse
#' @param width Field width
#'
#' @return String
#'
abbrDeparse <- function (x, width = 60)
{
  r <- deparse(x, width)
  if (length(r) > 1)
    paste(r[1], "...")
  else r
}

#' Extract the Number of Observations from a galamm Fit
#'
#' @param object galamm object
#' @param ... other arguments
#'
#' @return A number
#' @export
#'
nobs.galamm <- function(object, ...){
  object$n
}
