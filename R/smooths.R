##' @importFrom mgcv s
##' @export s
NULL

##' @importFrom mgcv t2
##' @export t2
NULL

#' Set up smooth term with factor loading
#'
#' This is a very thin wrapper around \code{mgcv::s}. It enables the
#' specification of loading variables for smooth terms. The last letter "l",
#' which stands for "loading", has been added to avoid namespace conflicts
#' with \code{mgcv} and \code{gamm4}.
#'
#' @param ... Arguments passed on to \code{mgcv::s}.
#' @param load.var Optional character argument specifying the loading variable.
#'
#' @return An object of class \code{xx.smooth.spec}, where \code{xx} is a basis
#'   identifying code given by the \code{bs} argument of \code{s}.
#'   It differs from the smooth returned by \code{mgcv::s} in
#'   that it has an additional attribute named \code{"load.var"} which specifies
#'   any factor loading which this smooth term should be multiplied with in
#'   order to produce the observed outcome.
#'
#' @export
#' @family modeling functions
#'
#' @references
#'
#' \insertRef{woodThinPlateRegression2003}{galamm}
#'
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
sl <- function(..., load.var = NULL) {
  ret <- s(...)
  attr(ret, "load.var") <- load.var
  ret
}

#' Set up smooth term with factor loading
#'
#' This is a very thin wrapper around \code{mgcv::t2}. It enables the
#' specification of loading variables for smooth terms. The last letter "l",
#' which stands for "loading", has been added to avoid namespace conflicts
#' with \code{mgcv} and \code{gamm4}.
#'
#' @param ... Arguments passed on to \code{mgcv::t2}.
#' @param load.var Optional character argument specifying the loading variable.
#'
#' @return An object of class \code{xx.smooth.spec}, where \code{xx} is a basis
#'   identifying code given by the \code{bs} argument of \code{t2}.
#'   It differs from the smooth returned by \code{mgcv::s} in
#'   that it has an additional attribute named \code{"load.var"} which specifies
#'   any factor loading which this smooth term should be multiplied with in
#'   order to produce the observed outcome.
#'
#' @export
#' @family modeling functions
#'
#' @references
#'
#' \insertRef{woodThinPlateRegression2003}{galamm}
#'
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
t2l <- function(..., load.var = NULL) {
  ret <- t2(...)
  attr(ret, "load.var") <- load.var
  ret
}
