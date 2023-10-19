#' @title Predictions from a model at new data values
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#' @srrstats {G2.3a} match.arg() used on "type" argument.
#' @srrstats {G2.3b} Argument "type" is case sensitive, which is documented.
#'
#' @description Predictions are given at the population level, i.e., with random
#'   effects set to zero. For fitted models including random effects, see
#'   \code{\link{fitted.galamm}}. For mixed response models, only predictions on
#'   the scale of the linear predictors is supported.
#'
#' @param object An object of class \code{galamm} returned from
#'   \code{\link{galamm}}.
#' @param newdata Data from for which to evaluate predictions, in a
#'   \code{data.frame}. Defaults to "NULL", which means that the predictions are
#'   evaluate at the data used to fit the model.
#' @param type Character argument specifying the type of prediction object to be
#'   returned. Case sensitive.
#' @param ... Optional arguments passed on to other methods. Currently used for
#'   models with smooth terms, for which these arguments are forwarded to
#'   \code{mgcv::predict.gam}.
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
#' # Predict on a new dataset
#' nd <- data.frame(lbas = c(.3, .2), treat = c(0, 1), lage = 0.2, v4 = -.2)
#' predict(count_mod, newdata = nd)
#' predict(count_mod, newdata = nd, type = "response")
#'
predict.galamm <- function(object, newdata = NULL,
                           type = c("link", "response"),
                           ...) {
  type <- match.arg(type)

  if (!is.null(newdata)) {
    if (!is.null(object$gam) && length(object$gam) > 0) {
      return(predict(object$gam, newdata = newdata, type = type, ...))
    }
    newform <- stats::update(lme4::nobars(eval(object$call[[2]])), NULL ~ .)
    X <- stats::model.matrix(newform, data = newdata)
    beta_hat <-
      object$parameters$parameter_estimates[object$parameters$beta_inds]

    linear_predictor <- X %*% beta_hat
  } else {
    if (!is.null(object$gam) && length(object$gam) > 0) {
      return(predict(object$gam, type = type, ...))
    }
    linear_predictor <- family(object)[[1]]$linkfun(object$model$fit_population)
  }
  if (length(object$model$family) > 1 && type == "response") {
    stop("For mixed response model, only type='link' works.")
  }

  if (type == "response") {
    family(object)[[1]]$linkinv(linear_predictor)
  } else {
    linear_predictor
  }
}
