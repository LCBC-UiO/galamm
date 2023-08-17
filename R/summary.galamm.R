#' Summarizing GALAMM Fits
#'
#' Summary method for class "galamm".
#'
#' @param object An object of class "galamm", usually the result of calling
#' \code{\link{galamm}}.
#' @param ... Further arguments.
#'
#' @return A list of summary statistics of the fitted model.
#' @export
#'
summary.galamm <- function(object, ...) {
  ret <- object
  class(ret) <- append("summary.galamm", class(object))

  ret$AICtab <- c(
    AIC = object$deviance + 2 * object$df,
    BIC = object$deviance + object$df * log(object$n),
    logLik = object$loglik,
    deviance = object$deviance,
    df.resid = object$n - object$df
  )

  ret$Lambda <- factor_loadings(object)

  useSc <- Reduce(function(`&&`, x) x()$family == "gaussian",
    object$family,
    init = TRUE
  )
  ret$VarCorr <- structure(
    lme4::mkVarCorr(sqrt(ret$phi)[[1]], ret$cnms,
      nc = lengths(ret$cnms),
      theta = ret$par[ret$theta_inds], names(ret$cnms)
    ),
    useSc = useSc,
    class = "VarCorr.merMod"
  )

  if (!is.null(object$weights_obj)) {
    ret$weights <- c(1, object$par[object$weights_inds])
    names(ret$weights) <- levels(object$weights_obj$flist[[1]])
  }

  ret$fixef <- fixef(object)
  ret$fixef <- cbind(
    Estimate = fixef(object),
    `Std. Error` = sqrt(diag(vcov(object, "beta")))
  )
  ret$fixef <- cbind(ret$fixef, (cf3 <- ret$fixef[, 1] / ret$fixef[, 2]),
    deparse.level = 0
  )
  colnames(ret$fixef)[3] <- paste(if (useSc) "t" else "z", "value")
  ret$fixef <- cbind(ret$fixef, 2 * pnorm(abs(cf3), lower.tail = FALSE))
  colnames(ret$fixef)[4] <- paste("Pr(>|", substr(colnames(ret$fixef)[3], 1, 1), "|)", sep = "")
  rownames(ret$fixef) <- object$par_names[object$beta_inds]

  ret
}


#' Print method for GALAMM fits
#'
#' @param x An object of class "summary.galamm".
#' @param digits Number of digits.
#' @param ... Further arguments.
#'
#' @return Summary printed to screen. Invisible returns the argument \code{x}.
#' @export
print.summary.galamm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Generalized additive latent and mixed model fit by maximum marginal likelihood.\n")
  lme4::.prt.call(x$mc)
  lme4::.prt.family(x$family)
  cat("\n")
  lme4::.prt.aictab(x$AICtab)
  cat("\n")
  lme4::.prt.resids(x$pearson_residuals / sqrt(x$phi), digits = digits)
  if (exists("Lambda", x)) {
    cat("Lambda:\n")
    x$Lambda[x$Lambda == 0] <- NA
    print(x$Lambda, digits = digits, na.print = ".")
    cat("\n")
  }
  lme4::.prt.VC(x$VarCorr, digits = digits, comp = c("Var", "Std.Dev."))
  lme4::.prt.grps(vapply(x$lmod$reTrms$flist, nlevels, 1), x$n)
  cat("\n")
  if (exists("weights", x)) {
    cat("Variance function:\n")
    print(x$weights, digits = digits, na.print = ".")
    cat("\n")
  }
  cat("Fixed effects:\n")
  print(x$fixef, digits = digits)
  invisible(x)
}
