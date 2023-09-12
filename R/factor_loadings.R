factor_loadings <- function(object) {
  UseMethod("factor_loadings")
}

#' Extract factor loadings from galamm object
#'
#' @param object Object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#'
#' @return A matrix containing the estimated factor loadings with corresponding
#'   standard deviations.
#' @export
#'
#' @details This function has been named \code{factor_loadings} rather than just
#'   \code{loadings} to avoid conflict with \code{stats::loadings}.
#'
#' @seealso [fixef.galamm()] for fixed regression coefficients,
#'   [confint.galamm()] for confidence intervals, and [coef.galamm()] for
#'   coefficients more generally.
#'
factor_loadings.galamm <- function(object) {
  if (!exists("lambda", object)) {
    return(invisible(NULL))
  }

  lambda_tmp_est <- lambda_tmp_se <- object$lambda[[1]]
  lambda_tmp_se[lambda_tmp_se %in% c(0, 1)] <- NA_real_

  lambda_tmp_est[lambda_tmp_est > 1] <- object$par[object$lambda_inds]
  lambda_tmp_se[!is.na(lambda_tmp_se)] <-
    sqrt(diag(vcov(object, parm = object$lambda_inds)))

  matrix(rbind(lambda_tmp_est, lambda_tmp_se),
    nrow = nrow(lambda_tmp_est),
    dimnames = list(
      paste0("lambda", seq_len(nrow(object$lambda[[1]]))),
      as.character(rbind(colnames(lambda_tmp_est), "SE"))
    )
  )
}
