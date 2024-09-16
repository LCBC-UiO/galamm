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

#' @title Set up smooth term with factor loading
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.3,G2.3b} Argument "factor" is case sensitive, as is documented here.
#' @srrstats {G2.5} No inputs are explicitly expected to be of factor type in
#'   this function, but such expectations exist for arguments forwarded to
#'   \code{mgcv::s}. This is elaborated under the "Details" heading in the
#'   documentation.
#'
#' @description This is a very thin wrapper around \code{mgcv::s}. It enables
#' the specification of loading variables for smooth terms. The last letter "l",
#' which stands for "loading", has been added to avoid namespace conflicts with
#' \code{mgcv} and \code{gamm4}.
#'
#' @param ... Arguments passed on to \code{mgcv::s}.
#' @param factor Optional character argument specifying the loading variable.
#'   Case sensitive.
#'
#' @return An object of class \code{xx.smooth.spec}, where \code{xx} is a basis
#'   identifying code given by the \code{bs} argument of \code{s}. It differs
#'   from the smooth returned by \code{mgcv::s} in that it has an additional
#'   attribute named \code{"factor"} which specifies any factor loading which
#'   this smooth term should be multiplied with in order to produce the observed
#'   outcome.
#'
#' @details The documentation of the function \code{mgcv::s} should be consulted
#'   for details on how to properly set up smooth terms. In particular, note
#'   that these terms distinguish between ordered and unordered factor terms
#'   in the \code{by} variable, which can be provided in \code{...} and is
#'   forwarded to \code{mgcv::s}.
#'
#' @export
#' @family modeling functions
#'
#' @references
#'
#' \insertRef{woodThinPlateRegression2003}{galamm}
#'
#' \insertRef{woodGeneralizedAdditiveModels2017}{galamm}
#'
#' @examples
#' # Linear mixed model with factor structures
#' dat <- subset(cognition, domain == 1 & timepoint == 1)
#' loading_matrix <- matrix(c(1, NA, NA), ncol = 1)
#'
#'
#' # Model with four thin-plate regression splines as basis functions
#' mod <- galamm(
#'   formula = y ~ 0 + item + sl(x, k = 4, factor = "loading"),
#'   data = dat,
#'   load.var = "item",
#'   lambda = loading_matrix,
#'   factor = "loading"
#' )
#'
#' # Model with four cubic regression splines as basis functions
#' mod <- galamm(
#'   formula = y ~ 0 + item +
#'     sl(x, bs = "cr", k = 4, factor = "loading"),
#'   data = dat,
#'   load.var = "item",
#'   lambda = loading_matrix,
#'   factor = "loading"
#' )
#'
sl <- function(..., factor = NULL) {
  ret <- s(...)
  attr(ret, "factor") <- factor
  ret
}

#' @title Set up smooth term with factor loading
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.3,G2.3b} Argument "factor" is case sensitive, as is documented here.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#' @srrstats {G2.5} No inputs are explicitly expected to be of factor type in
#'   this function, but such expectations exist for arguments forwarded to
#'   \code{mgcv::t2}. This is elaborated under the "Details" heading in the
#'   documentation.
#'
#' @description This is a very thin wrapper around \code{mgcv::t2}. It enables
#'   the specification of loading variables for smooth terms. The last letter
#'   "l", which stands for "loading", has been added to avoid namespace
#'   conflicts with \code{mgcv} and \code{gamm4}.
#'
#' @param ... Arguments passed on to \code{mgcv::t2}.
#' @param factor Optional character of length one specifying the loading
#'   variable. Case sensitive.
#'
#' @return An object of class \code{xx.smooth.spec}, where \code{xx} is a basis
#'   identifying code given by the \code{bs} argument of \code{t2}. It differs
#'   from the smooth returned by \code{mgcv::s} in that it has an additional
#'   attribute named \code{"factor"} which specifies any factor loading which
#'   this smooth term should be multiplied with in order to produce the observed
#'   outcome.
#'
#' @export
#' @family modeling functions
#'
#' @details The documentation of the function \code{mgcv::t2} should be consulted
#'   for details on how to properly set up smooth terms. In particular, note
#'   that these terms distinguish between ordered and unordered factor terms
#'   in the \code{by} variable, which can be provided in \code{...} and is
#'   forwarded to \code{mgcv::t2}.
#'
#' @references
#'
#' \insertRef{woodThinPlateRegression2003}{galamm}
#'
#' \insertRef{woodGeneralizedAdditiveModels2017}{galamm}
#'
#' @examples
#' # Linear mixed model with factor structures
#' dat <- subset(cognition, domain == 1 & timepoint == 1)
#' loading_matrix <- matrix(c(1, NA, NA), ncol = 1)
#'
#' # Model with four cubic regression splines as basis functions
#' mod <- galamm(
#'   formula = y ~ 0 + item + t2l(x, k = 4, factor = "loading"),
#'   data = dat,
#'   load.var = "item",
#'   lambda = loading_matrix,
#'   factor = "loading"
#' )
#'
#' # Model with four thin-plate regression splines as basis functions
#' mod <- galamm(
#'   formula = y ~ 0 + item +
#'     t2l(x, bs = "tp", k = 4, factor = "loading"),
#'   data = dat,
#'   load.var = "item",
#'   lambda = loading_matrix,
#'   factor = "loading"
#' )
#'
t2l <- function(..., factor = NULL) {
  ret <- t2(...)
  attr(ret, "factor") <- factor
  ret
}
