#' Predictions from a model at new data values
#'
#' Predictions are given at the population level, i.e., with random effects set
#' to zero. For fitted models including random effects, see
#' \code{\link{fitted.galamm}}. For mixed response models, only predictions on
#' the scale of the linear predictors is supported.
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param newdata Data from for which to evaluate predictions. Defaults to
#'   "NULL", which means that the predictions are evaluate at the data used to
#'   fit the model.
#' @param type Type of prediction object to be returned.
#' @param ... Optional arguments passed on to other methods. Currently not used.
#'
#' @return A numeric vector of predicted values.
#' @export
#'
#' @seealso [fitted.galamm()] for model fits, [residuals.galamm()] for
#'   residuals, and [predict()] for the generic function.
#'
#' @family details of model fit
#'
#' @examples
#' # Poisson GLMM
#' count_mod <- galamm(
#'   formula = y ~ lbas * treat + lage + v4 + (1 | subj),
#'   data = epilep, family = poisson
#' )
#'
#' # Plot response versus link:
#' plot(
#'   predict(count_mod, type = "link"),
#'   predict(count_mod, type = "response")
#' )
#'
predict.galamm <- function(object, newdata = NULL,
                           type = c("link", "response"),
                           ...) {
  type <- match.arg(type)
  if (!is.null(newdata)) {
    X <- model.matrix(lme4::nobars(eval(object$call[[2]])), data = newdata)
    beta_hat <-
      object$parameters$parameter_estimates[object$parameters$beta_inds]

    linear_predictor <- X %*% beta_hat
  } else {
    linear_predictor <- family(object)[[1]]$linkfun(object$model$fit_population)
  }
  if(length(object$model$family) > 1 && type == "response") {
    stop("For mixed response model, only type='link' works.")
  }

  if (type == "response") {
    family(object)[[1]]$linkinv(linear_predictor)
  } else {
    linear_predictor
  }
}
