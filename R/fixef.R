#' Extract Fixed Effects from galamm Object
#'
#' @param object An object
#' @param ... Other parameters
#'
#' @aliases fixef
#' @return Fixed effects
#' @export
#' @importFrom nlme fixef
fixef.galamm <- function(object, ...){
  ret <- object$par[object$beta_inds]
  names(ret) <- object$fixef_names
  ret
}
