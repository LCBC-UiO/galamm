#' Set up smooth term with factor loading
#'
#' Thes are very thin wrappers around \code{mgcv::s} and \code{mgcv::t2}. They
#' enable the specification of loading variables for smooth terms.
#'
#' @param ... Arguments passed on to \code{mgcv::s} or \code{mgcv::t2}.
#' @param load.var Optional character argument specifying the loading variable.
#'
#' @return An object of class \code{xx.smooth.spec}, where \code{xx} is a basis
#'   identifying code given by the \code{bs} argument of \code{s} or \code{t2}.
#'   It differs from the smooth returned by \code{mgcv::s} or \code{mgcv::t2} in
#'   that it has an additional attribute named \code{"load.var"} which specifies
#'   any factor loading which this smooth term should be multiplied with in
#'   order to produce the observed outcome.
#'
#' @export
#' @family {modeling functions}
#'
#' @references
#'
#' \insertRef{woodThinPlateRegression2003}{galamm}
#'
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
s <- function(..., load.var = NULL) {
  ret <- mgcv::s(...)
  attr(ret, "load.var") <- load.var
  ret
}

#' @rdname s
t2 <- function(..., load.var = NULL) {
  ret <- mgcv::t2()
  attr(ret, "load.var") <- load.var
  ret
}
