#' Example dataset with binomial response and two levels.
#'
#' Simulated dataset similar to the one used in Section 3.1 of
#' \insertCite{rabe-hesketh2005;textual}{galamm}.
#'
#' @format A data frame with 300 rows and 4 variables:
#' \describe{
#'   \item{id}{Subject ID}
#'   \item{y}{Number of successes}
#'   \item{trials}{Number of trials. Always equal to 1.}
#'   \item{x}{Explanatory variable}
#' }
#'
#' @references \insertAllCited{}
"dat1"

#' Example dataset with binomial response and two levels.
#'
#' Simulated dataset similar to the one used in Section 3.3 of
#' \insertCite{rabe-hesketh2005;textual}{galamm}. This dataset can be used
#' to fit models with random slopes.
#'
#' @format A data frame with 1800 rows and 4 variables:
#' \describe{
#'   \item{id}{Subject ID}
#'   \item{y}{Number of successes}
#'   \item{trials}{Number of trials. Always equal to 1.}
#'   \item{x}{Explanatory variable}
#' }
#'
#' @references \insertAllCited{}
"dat2"


#' Gauss-Hermite quadrature rules
#'
#' List of Gauss-Hermite quadrature rules generated using the \code{gaussquad}
#' package \insertCite{novomestky2013}{galamm}, and then scaled by multiplying the
#' locations by \eqn{\sqrt{2}} and the weights by \eqn{1/\sqrt{pi}}, thus
#' making sure the standard normal distribution is being integrated over.
#'
#' @format A list with 100 elements. The \eqn{k}th element contains the order
#' \eqn{n} quadrature rules for the Hermite polynomials of order \eqn{k}. Each
#' list element is a dataframe containing the following elements:
#' \describe{
#'   \item{x}{Location of quadrature point}
#'   \item{w}{Quadrature weight}
#' }
#'
#' @keywords internal
#' @references \insertAllCited{}
"hermite_quadrature"
