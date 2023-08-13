#' Predictions from a model at new data values
#'
#' @param object A fitted model object
#' @param newdata Data from for which to evaluate predictions. Defaults to
#'   "NULL", which means that the predictions are evaluate at the data used to
#'   fit the model.
#' @param type Type of prediction object to be returned.
#'
#' @return A number vector of predicted values.
#' @export
#'
predict.galamm <- function(object, newdata = NULL, type = c("link", "response")){
  object$family
}
