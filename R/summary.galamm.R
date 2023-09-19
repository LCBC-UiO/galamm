#' Summarizing GALAMM fits
#'
#' Summary method for class "galamm".
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param ... Further arguments passed on to other methods. Currently not used.
#'
#' @return A list of summary statistics of the fitted model, of class
#'   \code{summary.galamm}.
#' @export
#'
#' @author Some of the code for producing summary information has been derived
#'   from the summary methods of \code{mgcv} (author: Simon Wood) and
#'   \code{lme4} \insertCite{batesFittingLinearMixedEffects2015}{galamm}
#'   (authors: Douglas M. Bates, Martin Maechler, Ben Bolker, and Steve Walker).
#'
#' @seealso [print.summary.galamm()] for the print method and [summary()] for
#'   the generic.
#'
#' @examples
#' # Linear mixed model with heteroscedastic residuals
#' mod <- galamm(
#'     formula = y ~ x + (1 | id),
#'     weights = ~ (1 | item),
#'     data = hsced)
#'
#' summary(mod)
#'
summary.galamm <- function(object, ...) {
  ret <- object
  class(ret) <- append("summary.galamm", class(object))

  ret$AICtab <- llikAIC(object)
  ret$Lambda <- factor_loadings(object)
  ret$VarCorr <- VarCorr(ret)

  if (!is.null(object$model$weights_obj)) {
    ret$weights <- c(1, 1 / object$parameters$parameter_estimates[object$parameters$weights_inds])
    names(ret$weights) <- levels(object$model$weights_obj$flist[[1]])
  }

  ret$fixef <- fixef(object)
  ret$fixef <- cbind(
    Estimate = fixef(object),
    `Std. Error` = sqrt(diag(vcov(object, "beta")))
  )
  ret$fixef <- cbind(ret$fixef, (cf3 <- ret$fixef[, 1] / ret$fixef[, 2]),
    deparse.level = 0
  )
  colnames(ret$fixef)[3] <- paste(if (attr(ret$VarCorr, "useSc")) "t" else "z", "value")
  ret$fixef <- cbind(ret$fixef, 2 * pnorm(abs(cf3), lower.tail = FALSE))
  colnames(ret$fixef)[4] <- paste("Pr(>|", substr(colnames(ret$fixef)[3], 1, 1), "|)", sep = "")
  rownames(ret$fixef) <- object$parameters$parameter_names[object$parameters$beta_inds]

  if (!is.null(ret$gam)) {
    ret$gam_summary <- summary(ret$gam)
  }

  ret
}


#' Print method for summary GALAMM fits
#'
#'
#' @param x An object of class \code{summary.galamm} returned from
#'   \code{\link{summary.galamm}}.
#' @param digits Number of digits to present in outputs.
#' @param ... Further arguments passed on to other methods. Currently used by
#'   \code{stats::printCoefmat} for printing approximate significance of smooth
#'   terms.
#'
#' @return Summary printed to screen. Invisibly returns the argument \code{x}.
#' @export
#'
#' @author Some of the code for producing summary information has been derived
#'   from the summary methods of \code{mgcv} (author: Simon Wood) and
#'   \code{lme4} \insertCite{batesFittingLinearMixedEffects2015}{galamm}
#'   (authors: Douglas M. Bates, Martin Maechler, Ben Bolker, and Steve Walker).
#'
#' @seealso [summary.galamm()] for the summary function and [print()] for the
#'   generic function.
#'
#' @references \insertAllCited{}
#'
#' @examples
#' # Linear mixed model with heteroscedastic residuals
#' mod <- galamm(
#'     formula = y ~ x + (1 | id),
#'     weights = ~ (1 | item),
#'     data = hsced)
#'
#' summary(mod)
#'
print.summary.galamm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Generalized additive latent and mixed model fit by maximum marginal likelihood.\n")
  lme4::.prt.call(x$call)
  lme4::.prt.family(family(x))
  cat("\n")
  lme4::.prt.aictab(x$AICtab)
  cat("\n")
  lme4::.prt.resids(residuals(x) / sigma(x), digits = digits)
  if (exists("Lambda", x)) {
    cat("Lambda:\n")
    x$Lambda[x$Lambda == 0] <- NA
    print(x$Lambda, digits = digits, na.print = ".")
    cat("\n")
  }
  lme4::.prt.VC(x$VarCorr, digits = digits, comp = c("Var", "Std.Dev."))
  lme4::.prt.grps(vapply(x$model$lmod$reTrms$flist, nlevels, 1), nobs(x))
  cat("\n")
  if (exists("weights", x)) {
    cat("Variance function:\n")
    print(x$weights, digits = digits, na.print = ".")
    cat("\n")
  }
  cat("Fixed effects:\n")
  print(x$fixef, digits = digits)

  cat("\n")
  if (exists("gam_summary", x)) {
    cat("Approximate significance of smooth terms:\n")
    printCoefmat(x$gam_summary$s.table,
      digits = digits, signif.stars = FALSE,
      has.Pvalue = TRUE, na.print = "NA", cs.ind = 1, ...
    )
  }
  cat("\n")

  invisible(x)
}


llikAIC <- function(object) {
  UseMethod("llikAIC")
}


#' Extract log likelihood, AIC, and related statistics from a GALAMM
#'
#' This function is assembles the values used by \code{\link{summary.galamm}}.
#'
#' @param object Object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#'
#' @return A list containing AIC, BIC, log likelihood, deviance and residual
#'   degrees of freedom.
#' @export
#'
llikAIC.galamm <- function(object) {
  llik <- logLik(object)
  c(
    AIC = AIC(llik),
    BIC = BIC(llik),
    logLik = llik,
    deviance = deviance(object),
    df.resid = nobs(object) - object$model$df
  )
}
