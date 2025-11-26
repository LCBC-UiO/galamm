#' Extract the model frame from a galamm object
#'
#' @param formula An object of type `galamm` as described in [galammObject].
#' @param ... Other arguments. Currently not used.
#'
#' @returns A data frame.
#' @export
#' @family details of model fit
model.frame.galamm <- function(formula, ...) {
  if (!is.null(formula$data)) {
    return(formula$data)
  }
  stop("formula does not contain data slot")
}
