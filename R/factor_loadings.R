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
#' @author The example for this function comes from \code{PLmixed}, with
#'   authors Nicholas Rockwood and Minjeong Jeon
#'   \insertCite{rockwoodEstimatingComplexMeasurement2019}{galamm}.
#'
#' @family details of model fit
#'
#' @examples
#' # Logistic mixed model with factor loadings, example from PLmixed
#' data("IRTsim", package = "PLmixed")
#'
#' # Reduce data size for the example to run faster
#' IRTsub <- IRTsim[IRTsim$item < 4, ]
#' IRTsub <- IRTsub[sample(nrow(IRTsub), 300), ]
#' IRTsub$item <- factor(IRTsub$item)
#'
#' # Fix loading for first item to 1, and estimate the two others freely
#' loading_matrix <- matrix(c(1, NA, NA), ncol = 1)
#'
#' # Estimate model
#' mod <- galamm(y ~ item + (0 + ability | sid) + (0 + ability | school),
#'   data = IRTsub, family = binomial, load.var = "item",
#'   factor = "ability", lambda = loading_matrix
#' )
#'
#' # Show estimated factor loadings, with standard errors
#' factor_loadings(mod)
#'
factor_loadings.galamm <- function(object) {
  if (is.null(object$parameters$lambda_dummy)) {
    return(invisible(NULL))
  }

  lambda_tmp_est <- lambda_tmp_se <- object$parameters$lambda_dummy
  lambda_tmp_se[lambda_tmp_se %in% c(0, 1)] <- NA_real_

  lambda_tmp_est[lambda_tmp_est > 1] <-
    object$parameters$parameter_estimates[object$parameters$lambda_inds]
  lambda_tmp_se[!is.na(lambda_tmp_se)] <-
    sqrt(diag(vcov(object, parm = "lambda")))

  nn <- nrow(object$parameters$lambda_dummy)
  ret <- matrix(rbind(lambda_tmp_est, lambda_tmp_se),
    nrow = nrow(lambda_tmp_est),
    dimnames = list(
      paste0("lambda", seq_len(nn)),
      as.character(rbind(colnames(lambda_tmp_est), "SE"))
    )
  )

  lix <- length(object$parameters$lambda_interaction_inds)
  if (lix > 0) {
    vars <- unlist(lapply(object$model$factor_interactions, function(x) {
      attr(stats::terms(x), "term.labels")
    }))

    ret2 <- matrix(
      c(
        object$parameters$parameter_estimates[
          object$parameters$lambda_interaction_inds
        ],
        sqrt(diag(vcov(object, parm = "lambda_interaction")))
      ),
      nrow = lix
    )
    rownames(ret2) <- paste0(
      "lambda", seq(from = nn + 1, length.out = lix),
      "_", vars
    )

    ret <- rbind(ret, ret2)
  }
  ret
}
