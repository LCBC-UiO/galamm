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
summary.galamm <- function(object, ...){
  ret <- object
  class(ret) <- append("summary.galamm", class(object))

  ret$AICtab <- c(
    AIC = object$deviance + 2 * object$df,
    BIC = object$deviance + object$df * log(object$n),
    logLik = object$loglik,
    deviance = object$deviance,
    df.resid = object$n - object$df
  )

  lambda_tmp_est <- lambda_tmp_se <- object$lambda[[1]]
  to_fill <- lambda_tmp_est > 0
  lambda_tmp_est[to_fill] <- c(1, object$par[object$lambda_inds])[lambda_tmp_est[to_fill]]
  lambda_tmp_se[to_fill] <- c(NA_real_, sqrt(diag(object$vcov[object$lambda_inds, object$lambda_inds])))[lambda_tmp_se[to_fill]]

  ret$Lambda <- matrix(rbind(lambda_tmp_est, lambda_tmp_se), nrow = nrow(lambda_tmp_est),
         dimnames = list(NULL, as.character(rbind(colnames(lambda_tmp_est), "SE"))))

  rownames(ret$Lambda) <- seq_len(nrow(ret$Lambda))

  ret$VarCorr <- structure(
    lme4::mkVarCorr(sqrt(ret$phi), ret$cnms, nc = lengths(ret$cnms),
                    theta = ret$par[ret$theta_inds], names(ret$cnms)),
    useSc = TRUE, class = "VarCorr.merMod")

  ret$fixef <- cbind(
    Estimate = object$par[object$beta_inds],
    `Std. Error` = sqrt(diag(object$vcov[object$beta_inds, object$beta_inds]))
      )
  ret$fixef <- cbind(ret$fixef, `t value` = ret$fixef[, 1] / ret$fixef[, 2])
  rownames(ret$fixef) <- object$fixef_names

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
print.summary.galamm <- function(x, digits = max(3, getOption("digits") - 3), ...){
  cat("Generalized additive latent and mixed model fit by maximum marginal likelihood.\n")
  lme4::.prt.call(x$mc)
  lme4::.prt.family(x$family)
  cat("\n")
  lme4::.prt.aictab(x$AICtab)
  cat("\n")
  lme4::.prt.resids(x$residuals / sqrt(x$phi), digits = digits)
  cat("Lambda:\n")
  x$Lambda[x$Lambda == 0] <- NA
  print(x$Lambda, digits = digits, na.print = ".")
  cat("\n")
  lme4::.prt.VC(x$VarCorr, digits = digits, comp = c("Var", "Std.Dev."))
  lme4::.prt.grps(vapply(x$lmod$reTrms$flist, nlevels, 1), x$n)
  cat("\n")
  cat("Fixed effects:\n")
  print(x$fixef, digits = digits)
  invisible(x)
}
