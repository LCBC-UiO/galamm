#' @keywords internal
#' @aliases galamm-package NULL
#'
#' @references
#'
#' \insertRef{sorensenLongitudinalModelingAgeDependent2023}{galamm}
#'
"_PACKAGE"

## usethis namespace: start
#' @useDynLib galamm, .registration = TRUE
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @importFrom stats anova coef deviance family fitted formula gaussian logLik
#' nobs predict residuals sigma vcov
#' @importFrom Rdpack reprompt
## usethis namespace: end
NULL
