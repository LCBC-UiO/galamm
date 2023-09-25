##' @importFrom mgcv s
##' @export s
##' @name s
##' @rdname sl
NULL

##' @importFrom mgcv t2
##' @export t2
##' @name t2
##' @rdname t2l
NULL

#' Set up smooth term with factor loading
#'
#' This is a very thin wrapper around \code{mgcv::s}. It enables the
#' specification of loading variables for smooth terms. The last letter "l",
#' which stands for "loading", has been added to avoid namespace conflicts with
#' \code{mgcv} and \code{gamm4}.
#'
#' @param ... Arguments passed on to \code{mgcv::s}.
#' @param load.var Optional character argument specifying the loading variable.
#'
#' @return An object of class \code{xx.smooth.spec}, where \code{xx} is a basis
#'   identifying code given by the \code{bs} argument of \code{s}. It differs
#'   from the smooth returned by \code{mgcv::s} in that it has an additional
#'   attribute named \code{"load.var"} which specifies any factor loading which
#'   this smooth term should be multiplied with in order to produce the observed
#'   outcome.
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
#' @examples
#' # Linear mixed model with factor structures
#' dat <- subset(cognition, domain == 1 & timepoint == 1)
#' loading_matrix <- matrix(c(1, NA, NA), ncol = 1)
#'
#' # Model with four thin-plate regression splines as basis functions
#' mod <- galamm(
#'   formula = y ~ 0 + item + sl(x, k = 4, load.var = "loading"),
#'   data = dat,
#'   load.var = "item",
#'   lambda = list(loading_matrix),
#'   factor = list("loading")
#' )
#'
#' # Model with four cubic regression splines as basis functions
#' mod <- galamm(
#'   formula = y ~ 0 + item +
#'     sl(x, bs = "cr", k = 4, load.var = "loading"),
#'   data = dat,
#'   load.var = "item",
#'   lambda = list(loading_matrix),
#'   factor = list("loading")
#' )
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
#' @examples
#' # Linear mixed model with factor structures
#' dat <- subset(cognition, domain == 1 & timepoint == 1)
#' loading_matrix <- matrix(c(1, NA, NA), ncol = 1)
#'
#' # Model with four cubic regression splines as basis functions
#' mod <- galamm(
#'   formula = y ~ 0 + item + t2l(x, k = 4, load.var = "loading"),
#'   data = dat,
#'   load.var = "item",
#'   lambda = list(loading_matrix),
#'   factor = list("loading")
#' )
#'
#' # Model with four thin-plate regression splines as basis functions
#' mod <- galamm(
#'   formula = y ~ 0 + item +
#'     sl(x, bs = "tp", k = 4, load.var = "loading"),
#'   data = dat,
#'   load.var = "item",
#'   lambda = list(loading_matrix),
#'   factor = list("loading")
#' )
#'
t2l <- function(..., load.var = NULL) {
  ret <- t2(...)
  attr(ret, "load.var") <- load.var
  ret
}
