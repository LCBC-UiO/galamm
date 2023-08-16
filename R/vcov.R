#' Calculate Variance-Covariance Matrix for galamm Model Object
#'
#' @param object Fitted model
#' @param parm The parameters for which the variance-covariance matrix should
#' be calculated. Character vector with one or more of the elements "theta",
#' "beta", "lambda", and "weights". Can also be an integer vector.
#' @param ... Further arguments
#'
#' @return Variance-covariance matrix
#' @export
#'
vcov.galamm <- function(object, parm = "beta", ...){

  inds <- find_parm_inds(object, parm)
  # Important: invert Hessian before subsetting. Otherwise uncertainty will
  # be too low.
  -solve(object$hessian)[inds, inds, drop = FALSE]
}


find_parm_inds <- function(object, parm){
  if(is.integer(parm)){
    parm
  } else if(is.character(parm)){
    Reduce(function(x, y) {
      c(x, eval(parse(text = paste0("object$", y, "_inds"))))
    }, parm, init = integer())
  } else {
    stop("parm must be an integer or character vector")
  }
}
