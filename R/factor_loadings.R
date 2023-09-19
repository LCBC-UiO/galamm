factor_loadings <- function(object) {
  UseMethod("factor_loadings")
}

#' Extract factor loadings from galamm object
#'
#' @aliases factor_loadings factor_loadings.galamm
#' @export factor_loadings
#' @export
#'
#' @param object Object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#'
#' @return A matrix containing the estimated factor loadings with corresponding
#'   standard deviations.
#'
#' @details This function has been named \code{factor_loadings} rather than just
#'   \code{loadings} to avoid conflict with \code{stats::loadings}.
#'
#' @seealso [fixef.galamm()] for fixed regression coefficients,
#'   [confint.galamm()] for confidence intervals, and [coef.galamm()] for
#'   coefficients more generally.
#'
#' @examples
#' # Generalized additive mixed model with factor structures -------------------
#'
#' # The cognition dataset contains simulated measurements of three latent
#' # time-dependent processes, corresponding to individuals' abilities in
#' # cognitive domains. We focus here on the first domain, and subset the data
#' # accordingly.
#' dat <- subset(cognition, domain == 1)
#' dat$item <- factor(dat$item)
#'
#' # There are eight timepoints for each individual, and at each timepoint
#' # there are three items measuring ability in the cognitive domain. We fix
#' # the factor loading for the first measurement to one, and estimate the
#' # remaining two. This is specified in the loading matrix.
#' loading_matrix <- matrix(c(1, NA, NA), ncol = 1)
#'
#' # We can now estimate the model.
#' mod <- galamm(
#'     formula = y ~ 0 + item + s(x, load.var = "loading") +
#'               (0 + loading | id / timepoint),
#'     data = dat,
#'     load.var = "item",
#'     lambda = list(loading_matrix),
#'     factor = list("loading")
#'     )
#'
#' # Extract factor loadings
#' factor_loadings(mod)
#'
factor_loadings.galamm <- function(object) {
  if (is.null(object$parameters$lambda_dummy)) {
    return(invisible(NULL))
  }

  lambda_tmp_est <- lambda_tmp_se <- object$parameters$lambda_dummy[[1]]
  lambda_tmp_se[lambda_tmp_se %in% c(0, 1)] <- NA_real_

  lambda_tmp_est[lambda_tmp_est > 1] <- object$parameters$parameter_estimates[object$parameters$lambda_inds]
  lambda_tmp_se[!is.na(lambda_tmp_se)] <-
    sqrt(diag(vcov(object, parm = object$parameters$lambda_inds)))

  matrix(rbind(lambda_tmp_est, lambda_tmp_se),
    nrow = nrow(lambda_tmp_est),
    dimnames = list(
      paste0("lambda", seq_len(nrow(object$parameters$lambda_dummy[[1]]))),
      as.character(rbind(colnames(lambda_tmp_est), "SE"))
    )
  )
}
