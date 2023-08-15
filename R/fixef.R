#' Extract Fixed Effects from galamm Object
#'
#' @param object An object
#' @param ... Other parameters
#'
#' @return Fixed effects
#'
#' @aliases fixef fixef.galamm
#'
#' @importFrom nlme fixef
#' @export fixef
#' @method fixef galamm
#' @export
fixef.galamm <- function(object, ...){
  ret <- object$par[object$beta_inds]
  names(ret) <- object$fixef_names
  ret
}
