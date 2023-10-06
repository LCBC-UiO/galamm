#' Predictions from a model at new data values
#'
#' Predictions are given at the population level, i.e., with random effects set
#' to zero. For mixed response models, only predictions on the scale of the
#' linear predictiors is supported.
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
    stop("Not implemented yet")
  }
  if(length(object$model$family) > 1 && type == "response") {
    stop("For mixed response model, only type='link' works.")
  }

  if (type == "response") {
    fitted(object)
  } else {
    family(object)[[1]]$linkfun(fitted(object))
  }
}
