#' @srrstats {G5.2} All errors and warnings are tested.
#' @srrstats {G5.2a} Every message produced within R code by stop(),
#'   warning(), or message(), is unique.
#' @srrstats {G5.2b} All stop(), warning(), and message() calls are tested,
#'   as can be seen in the CodeCov report on GitHub.
#' @srrstats {G5.3} Tests have explicit expectations about return objects.
#' @srrstats {G5.4a} These are new methods, but they have been used in the
#'   paper Sørensen, Fjell, and Walhovd (2023), in which extensive simulation
#'   studies confirmed the correctness of the implementation. Furthermore, the
#'   simulated datasets, which are documented in "R/data.R" and exported, have
#'   known ground truth and we confirm in the vignettes that the obtained
#'   estimates are close to the true values.
#' @srrstats {G5.4b} Wherever there is overlapping functionality, results from
#'   galamm() have been confirmed to be identical to those of lme4::lmer() for
#'   linear mixed models, to those of lme4::glmer() for generalized linear
#'   mixed models with binomial or Poisson responses, to those of
#'   nlme::lme() for linear mixed models with heteroscedastic residuals, and
#'   to those of PLmixed::PLmixed() for linear mixed models with factor
#'   structures and generalized linear mixed models with factor structures.
#' @srrstats {G5.5} Random seed is set when simulating data, but the
#'   algorithms are determinstic, and hence don't depend on random numbers.
#' @srrstats {G5.6} Implemented in the tests, both through data simulated
#'   for this package, and through simulated data from PLmixed and lme4.
#' @srrstats {G5.6a} Tolerance in testthat() set to relatively high values,
#'   since the outcome is platform dependent.
#'
#' @noRd
NULL

library(testthat)
library(galamm)

test_check("galamm")
