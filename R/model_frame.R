#' Extract the model frame from a galamm object
#'
#' @description
#'   Returns the data frame that was used to fit the model. The function first
#'   looks for a component named `data` (added by the `galamm()` function).  If
#'   that component is missing it falls back to re‑evaluating the expression
#'   supplied in the original call (`call$data`).  This mirrors the behavior
#'   of `model.frame.lmerMod` and other modelling packages.
#'
#' @param formula An object of type `galamm` as described in [galammObject].
#' @param ...   Ignored – present for S3 compatibility.
#' @return A data.frame containing the variables that were used in the fit.
#' @family details of model fit
#' @export
model.frame.galamm <- function(formula, ...) {
  if (!is.null(formula$data)) {
    return(formula$data)
  }
  stop(
    "No data stored in the 'galamm' object and the original call cannot be ",
    "re‑evaluated.  Re‑fit the model with `galamm(..., data = <data.frame>)` ",
    "and make sure the returned object keeps a `data` component."
  )
}
