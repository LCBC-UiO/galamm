#' Fit a generalized additive latent and mixed model
#'
#' Fits a GALAMM using a profile likelihood algorithm.
#' \code{gamm4::gamm4} is used to fit the underling
#' generalized additive mixed model at each step.
#'
#' @param formula A formula for the fixed effects, including
#'   smooth terms.
#' @param random A formula for random effects.
#' @param family A GLM family.
#' @param data A data frame containing the variables used in the
#'   model.
#' @param load.var The variables which the factors load onto.
#' @param lambda A factor loading matrix, with row names corresponding
#'  to \code{load.var}.
#' @param factor The latent variable, or factor.
#' @param optim_method Passed on to \code{stats::optim}.
#' @param optim_control Passed on to \code{stats::optim}.
#' @param numDeriv_method Passed on to \code{numDeriv::jacobian}.
#'
#' @return An object of S3 class \code{galamm}.
#' @export
#'
#' @details
#'  See the documentation to \code{gamm4::gamm4} and
#' \code{PLmixed::PLmixed} for details about the syntax.
#'
#' @examples
#' # Example dataset with three measurements of a latent response
#' # Define loading matrix
#' load.mat <- matrix(c(1, NA, NA), ncol = 1)
#' dimnames(load.mat) <- list(c("item1", "item2", "item3"), NULL)
#' \dontrun{
#' mod <- galamm(
#'   formula = y ~ s(x, by = weight),
#'   random = ~(1|id),
#'   data = dat1,
#'   load.var = "item",
#'   lambda = load.mat,
#'   factor = "weight",
#'   optim_control = list(trace = 3)
#'   )
#'   }
galamm <- function(formula, random = NULL, family = stats::gaussian(),
                   data = list(), load.var = NULL, lambda = NULL,
                   factor = NULL, optim_method = "L-BFGS-B",
                   optim_control = list(), numDeriv_method = "simple"){

  gammstart <- NULL
  lambda_init <- stats::runif(sum(is.na(lambda)), 1, 2)

  # Optimize over non-missing variables in lambda
  opt <- stats::optim(lambda_init, function(x){
    ll <- lambda
    ll[is.na(ll)] <- x
    eval(parse(text = paste0("data$", factor, "<-ll[data$", load.var, ", ]")))

    mod <- gamm4::gamm4(
      formula = formula, random = random, family = family,
      start = gammstart, data = data, REML = FALSE
    )

    gammstart <<- list(theta = lme4::getME(mod$mer, "theta"),
                       fixef = lme4::getME(mod$mer, "fixef"))

    -as.numeric(stats::logLik(mod$mer))
  }, method = optim_method, control = optim_control, hessian = TRUE)

  lambda_est <- lambda
  lambda_est[is.na(lambda)] <- opt$par
  eval(parse(text = paste0("data$", factor, "<-lambda_est[data$", load.var, ", ]")))

  final_gamm <- gamm4::gamm4(
    formula = formula, random = random, family = family,
    start = gammstart, data = data)

  beta_hat <- stats::coef(final_gamm$gam)
  cov_lambda <- solve(opt$hessian)
  cov_beta_naive <- stats::vcov(final_gamm$gam)

  beta_jacobian <- numDeriv::jacobian(
    function(x){
      ll <- lambda
      ll[is.na(ll)] <- x
      eval(parse(text = paste0("data$", factor, "<-ll[data$", load.var, ", ]")))
      mod <- gamm4::gamm4(
        formula = formula, random = random, family = family,
        start = gammstart, data = data)
      stats::coef(mod$gam)
    }, x = lambda_est[is.na(lambda)],
    method = numDeriv_method
  )

  cov_beta <- cov_beta_naive + beta_jacobian %*% cov_lambda %*% t(beta_jacobian)

  res <- list(
    gamm4 = final_gamm,
    beta_jacobian = beta_jacobian,
    cov_beta_naive = cov_beta_naive,
    cov_beta = cov_beta,
    cov_lambda = cov_lambda,
    lambda_est = lambda_est
  )
  class(res) <- "galamm"

  res

}
