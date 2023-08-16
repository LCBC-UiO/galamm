#' Predictions from a model at new data values
#'
#' @param object A fitted model object
#' @param newdata Data from for which to evaluate predictions. Defaults to
#'   "NULL", which means that the predictions are evaluate at the data used to
#'   fit the model.
#' @param type Type of prediction object to be returned.
#' @param ... Other arguments.
#'
#' @return A number vector of predicted values.
#' @export
#'
predict.galamm <- function(object, newdata = NULL,
                           type = c("link", "response"),
                           ...) {
  type <- match.arg(type)
  if (!is.null(newdata)) {
    stop("Not implemented yet")
  }

  if (type == "response") {
    object$fit
  } else {
    object$family[[1]]()$linkfun(object$fit)
  }
}
