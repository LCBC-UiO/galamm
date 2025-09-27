#' Grouped families
#'
#' This function is inspired by a function of the same name in the `mgcv`
#' package \insertCite{woodGeneralizedAdditiveModels2017}{galamm}, and supports
#' setting up mixed response type models. When using this function, the response
#' variable must be supported as a two-column matrix, in which the first column
#' contains the response and the second column contains an index mapping the
#' response to the list elements provided in the argument `fl` to this function.
#'
#' @param fl A list of families. Currently \code{gaussian}, \code{binomial}, and
#'   \code{poisson} with canonical link functions are supported.
#'
#' @returns An object of class "galamm_extended_family".
#' @export
#'
#' @family modeling functions
#'
#' @references \insertAllCited{}
#'
gfam <- function(fl) {
  if (!is.list(fl)) stop("fl must be a list")

  ret <- lapply(fl, function(f) {
    if (is.character(f)) {
      return(eval(parse(text = f))())
    } else if (is.function(f)) {
      return(f())
    } else {
      return(f)
    }
  })
  class(ret) <- "galamm_extended_family"
  ret
}
