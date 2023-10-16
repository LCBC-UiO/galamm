#' Compare likelihoods of galamm objects
#'
#' Anova function for comparing different GALAMMs fitted on the same data.
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param ... Other fitted models of class \code{galamm}. Currently, if no
#'   models are provided in this argument, no table will be returned.
#'
#' @author Some of the source code for this function is adapted from
#'   \code{lme4:::anova.merMod}, with authors Douglas M. Bates, Martin Maechler,
#'   Ben Bolker, and Steve Walker.
#'
#'
#' @return A table with model comparison metric.
#' @export
#'
#' @references \insertRef{batesFittingLinearMixedEffects2015}{galamm}
#'
#' @seealso [summary.galamm()] for the summary method and [anova()] for the
#'   generic function.
#'
#' @family summary functions
#'
#' @examples
#' # Poisson GLMM
#' count_mod <- galamm(
#'   formula = y ~ lbas * treat + lage + v4 + (1 | subj),
#'   data = epilep, family = poisson
#' )
#'
#' # Model without interaction
#' count_mod0 <- galamm(
#'   formula = y ~ lbas + treat + lage + v4 + (1 | subj),
#'   data = epilep, family = poisson
#' )
#'
#' # Model comparison
#' anova(count_mod, count_mod0)
#'
anova.galamm <- function(object, ...) {
  mCall <- match.call(expand.dots = TRUE)
  dots <- list(...)
  modp <- vapply(dots, inherits, TRUE, "galamm")


  mNms <- vapply(
    as.list(mCall)[c(FALSE, TRUE, modp)],
    deparse1, ""
  )

  if (any(modp)) {
    mods <- c(list(object), dots[modp])
    nobs.vec <- vapply(mods, nobs, 1L)
    if (stats::var(nobs.vec) > 0) {
      stop("models were not all fitted to the same size of dataset")
    }
    mNms <- vapply(as.list(mCall)[c(FALSE, TRUE, modp)], deparse1, "")
    names(mods) <- mNms
    llks <- lapply(mods, logLik)
    ii <- order(npar <- vapply(llks, attr, FUN.VALUE = numeric(1), "df"))
    mods <- mods[ii]
    llks <- llks[ii]
    npar <- npar[ii]

    calls <- lapply(mods, stats::getCall)
    data <- lapply(calls, `[[`, "data")
    if (!all(vapply(data, identical, NA, data[[1]]))) {
      stop("all models must be fit to the same data object")
    }
    header <- paste("Data:", deparse(data[[1]]))

    llk <- unlist(llks)
    chisq <- 2 * pmax(0, c(NA, diff(llk)))
    dfChisq <- c(NA, diff(npar))

    val <- data.frame(
      npar = npar,
      AIC = vapply(llks, stats::AIC, 1),
      BIC = vapply(llks, stats::BIC, 1),
      logLik = llk,
      deviance = deviance(object), Chisq = chisq,
      Df = dfChisq,
      `Pr(>Chisq)` = ifelse(dfChisq == 0, NA,
        stats::pchisq(chisq, dfChisq, lower.tail = FALSE)
      ),
      row.names = names(mods), check.names = FALSE
    )
    class(val) <- c("anova", class(val))
    forms <- lapply(lapply(calls, `[[`, "formula"), deparse1)

    structure(
      val,
      heading = c(
        header, "Models:",
        paste(rep.int(names(mods), lengths(forms)), unlist(forms),
          sep = ": "
        )
      )
    )
  } else {
    message("ANOVA tables for galamm objects not implemented yet.")
  }
}

#' Extract the Number of Observations from a galamm Fit
#'
#' @param object galamm object
#' @param ... other arguments
#'
#' @return A number
#' @export
#'
#' @family details of model fit
#'
nobs.galamm <- function(object, ...) {
  object$model$n
}
