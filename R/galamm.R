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
#' @param load_var The variables which the factors load onto.
#' @param lambda A factor loading matrix, with row names corresponding
#'  to \code{load_var}.
#' @param factor The latent variable, or factor.
#' @param lambda_init Initial value for free factor loadings. Default to \code{NULL},
#' which means that they are randomly drawn from a uniform distribution with support
#' \eqn{[0,1]}.
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
#'   load_var = "item",
#'   lambda = load.mat,
#'   factor = "weight",
#'   lambda_init = c(2, .4),
#'   optim_control = list(trace = 3)
#'   )
#'   }
galamm <- function(formula, random = NULL, family = stats::gaussian(),
                   data = list(), load_var = NULL, lambda = NULL,
                   factor = NULL, lambda_init = NULL, optim_method = "L-BFGS-B",
                   optim_control = list(), numDeriv_method = "simple"){

  gammstart <- NULL
  if(is.null(lambda_init)){
    lambda_init <- stats::runif(sum(is.na(lambda)), 1, 2)
  } else {
    stopifnot(length(lambda_init) == sum(is.na(lambda)))
  }

  # Optimize over non-missing variables in lambda
  opt <- stats::optim(lambda_init, function(x){
    ll <- lambda
    ll[is.na(ll)] <- x
    eval(parse(text = paste0("data$", factor, "<-ll[data$", load_var, ", ]")))

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
  eval(parse(text = paste0("data$", factor, "<-lambda_est[data$", load_var, ", ]")))

  final_gamm <- gamm4::gamm4(
    formula = formula, random = random, family = family,
    start = gammstart, data = data, REML = FALSE)

  beta_hat <- stats::coef(final_gamm$gam)
  cov_lambda <- solve(opt$hessian)
  cov_beta_naive <- stats::vcov(final_gamm$gam)

  beta_jacobian <- numDeriv::jacobian(
    function(x){
      ll <- lambda
      ll[is.na(ll)] <- x
      eval(parse(text = paste0("data$", factor, "<-ll[data$", load_var, ", ]")))
      mod <- gamm4::gamm4(
        formula = formula, random = random, family = family,
        start = gammstart, data = data)
      stats::coef(mod$gam)
    }, x = lambda_est[is.na(lambda)],
    method = numDeriv_method
  )

  cov_beta <- cov_beta_naive + beta_jacobian %*% cov_lambda %*% t(beta_jacobian)

  # find indices of spline coefficients
  inds <- integer(0L)
  for(sm in final_gamm$gam$smooth){
    inds <- c(inds, seq(sm$first.para, sm$last.para))
  }

  fixed_effects <- cbind(
    stats::coef(final_gamm$gam)[-inds, drop = FALSE],
    sqrt(diag(cov_beta[-inds, -inds, drop = FALSE])))
  colnames(fixed_effects) <- c("Beta", "SE")

  res <- list(
    gamm4 = final_gamm,
    cov_beta = cov_beta,
    cov_lambda = cov_lambda,
    lambda_est = lambda_est,
    load_var = load_var,
    loadings_estimated = length(opt$par),
    log_likelihood = stats::logLik(final_gamm$mer),
    formula = formula,
    iterations = opt$counts[["function"]],
    random_effects = lme4::VarCorr(final_gamm$mer),
    fixed_effects = fixed_effects
  )
  class(res) <- "galamm"

  res

}