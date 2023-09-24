#' Fit a generalized additive latent and mixed model
#'
#' This function fits a generalized additive latent and mixed model (GALAMMs),
#' as described in
#' \insertCite{sorensenLongitudinalModelingAgeDependent2023;textual}{galamm}.
#' The building blocks of these models are generalized additive mixed models
#' (GAMMs) \insertCite{woodGeneralizedAdditiveModels2017a}{galamm}, of which
#' generalized linear mixed models
#' \insertCite{breslowApproximateInferenceGeneralized1993,harvilleMaximumLikelihoodApproaches1977,hendersonBestLinearUnbiased1975,lairdRandomEffectsModelsLongitudinal1982}{galamm}
#' are special cases. GALAMMs extend upon GAMMs by allowing factor structures,
#' as commonly used to model hypothesized latent traits underlying observed
#' measurements. In this sense, GALAMMs are an extension of generalized linear
#' latent and mixed models (GLLAMMs)
#' \insertCite{skrondalGeneralizedLatentVariable2004,rabe-heskethGeneralizedMultilevelStructural2004}{galamm}
#' which allows semiparametric estimation. The implemented algorithm used to
#' compute model estimates is described in
#' \insertCite{sorensenLongitudinalModelingAgeDependent2023;textual}{galamm},
#' and is an extension of the algorithm used for fitting generalized linear
#' mixed models by the \code{lme4} package
#' \insertCite{batesFittingLinearMixedEffects2015}{galamm}. The syntax used to
#' define factor structures is based on that used by the \code{PLmixed} package,
#' which is detailed in
#' \insertCite{rockwoodEstimatingComplexMeasurement2019;textual}{galamm}.
#'
#' @param formula A formula specifying the model. Smooth terms are defined in
#'   the style of the \code{mgcv} and \code{gamm4} packages, see
#'   \insertCite{woodGeneralizedAdditiveModels2017a}{galamm} for an
#'   introduction. Random effects are specified using \code{lme4} syntax,
#'   which is described in detail in
#'   \insertCite{batesFittingLinearMixedEffects2015}{galamm}. Factor loadings
#'   will also be part of the model formula, and is based on the syntax of the
#'   \code{PLmixed} package
#'   \insertCite{rockwoodEstimatingComplexMeasurement2019}{galamm}.
#'
#' @param weights An optional formula object specifying an expression for the
#'   residual variance. Defaults to \code{NULL}, corresponding to homoscedastic
#'   errors. The formula is defined in \code{lme4} style; see vignettes and
#'   examples for details.
#'
#' @param data A dataset containing all the variables specified by the model
#'   formula, with the exception of factor loadings.
#'
#' @param family A vector containing one or more model families. For each
#'   element in \code{family} there should be a corresponding element in
#'   \code{family_mapping} specifying which elements of the response are
#'   conditionally distributed according to the given family. Currently family
#'   can be one of \code{gaussian}, \code{binomial}, and \code{poisson}, and
#'   only canonical link functions are supported.
#'
#' @param family_mapping A vector mapping from the elements of \code{family} to
#'   rows of \code{data}. Defaults to \code{rep(1L, nrow(data))}, which means
#'   that all observations are distributed according to the first element of
#'   \code{family}.
#'
#' @param load.var Character specifying the name of the variable in \code{data}
#'   identifying what the factors load onto. That is, each unique value of
#'   \code{load.var} corresponds to a unique factor loading. Currently only a
#'   single loading
#'
#' @param lambda List of factor loading matrices. Numerical values indicate that
#'   the given value is fixed, while \code{NA} means that the entry is a
#'   parameter to be estimated.
#'
#' @param factor List of character vectors identical to the factor loadings
#'   specified in \code{formula}. For each list element, the \eqn{j}th entry in
#'   the character vector corresponds to the \eqn{j}th column of the
#'   corresponding matrix in \code{lambda}.
#'
#' @param start A named list of starting values for parameters. Possible names
#'   of list elements are \code{"theta"}, \code{"beta"}, \code{"lambda"}, and
#'   \code{"weights"}, all of should be numerical vectors with starting values.
#'   Default to \code{NULL}, which means that some relatively sensible defaults
#'   are used.
#'
#' @param control Control object for the optimization procedure of class
#'   \code{galamm_control} resulting from calling \code{\link{galamm_control}}.
#'
#' @return A model object of class \code{galamm}, containing the following
#'   elements:
#' * \code{call} the matched call used when fitting the model.
#' * \code{random_effects} a list containing the following two elements:
#'   * \code{b} random effects in original parametrization.
#'   * \code{u} random effects standardized to have identity covariance matrix.
#' * \code{model} a list with various elements related to the model setup and
#'   fit:
#'   * \code{deviance} deviance of final model.
#'   * \code{deviance_residuals} deviance residuals of the final model.
#'   * \code{df} degrees of freedom of model.
#'   * \code{family} a list of one or more family objects, as specified in the
#'   \code{family} arguments to \code{galamm}.
#'   * \code{fit} a numeric vector with fitted values.
#'   * \code{hessian} Hessian matrix of final model, i.e., the second
#'   derivative of the log-likelihood with respect to all model parameters.
#'   * \code{lmod} Linear model object returned by \code{lme4::lFormula}, which
#'   is used internally for setting up the models.
#'   * \code{loglik} Log-likelihood of final model.
#'   * \code{n} Number of observations.
#'   * \code{pearson_residual} Pearson residuals of final model.
#'   * \code{reduced_hessian} Logical specifying whether the full Hessian
#'   matrix was computed, or a Hessian matrix with derivatives only with
#'   respect to beta and lambda.
#'   * \code{response} A numeric vector containing the response values used when
#'   fitting the model.
#'   * \code{weights_object} Object with weights used in model fitting. Is
#'   \code{NULL} when no weights were used.
#' * \code{parameters} A list object with model parameters and related
#'   information:
#'   * \code{beta_inds} Integer vector specifying the indices of fixed
#'   regression coefficients among the estimated model parameters.
#'   * \code{dispersion_parameter} One or more dispersion parameters of the
#'   final model.
#'   * \code{lambda_dummy} Dummy matrix of factor loadings, which shows the
#'   structure of the loading matrix that was supplied in the \code{lambda}
#'   arguments.
#'   * \code{lambda_inds} Integer vector specifying the indices of factor
#'   loadings among the estimated model parameters.
#'   * \code{parameter_estimates} Numeric vector of final parameter estimates.
#'   * \code{parameter_names} Names of all parameters estimates.
#'   * \code{theta_inds} Integer vector specifying the indices of variance
#'   components among the estimated model parameters. Technically these are the
#'   entries of the Cholesky decomposition of the covariance matrix.
#'   * \code{weights_inds} Integer vector specifying the indices of estimated
#'   weights (used in heteroscedastic Gaussian models) among the estimated
#'   model parameters.
#'  * \code{gam} List containing information about smooth terms in the model.
#'  If no smooth terms are contained in the model, then it is a list of length
#'  zero.
#'
#' @export
#'
#' @references \insertAllCited{}
#'
#' @examples
#' # Mixed response model ------------------------------------------------------
#'
#' # The mresp dataset contains a mix of binomial and Gaussian responses.
#'
#' # We need to estimate a factor loading which scales the two response types.
#' loading_matrix <- matrix(c(1, NA), ncol = 1)
#'
#' # Define mapping to families.
#' families <- c(gaussian, binomial)
#' family_mapping <- ifelse(mresp$itemgroup == "a", 1, 2)
#'
#'
#' # Fit the model
#' mod <- galamm(
#'   formula = y ~ x + (0 + level | id),
#'   data = mresp,
#'   family = families,
#'   family_mapping = family_mapping,
#'   factor = list("level"),
#'   load.var = "itemgroup",
#'   lambda = list(loading_matrix)
#' )
#'
#' # Summary information
#' summary(mod)
#'
#'
#' # Heteroscedastic model -----------------------------------------------------
#' # Residuals allowed to differ according to the item variable
#' mod <- galamm(formula = y ~ x + (1 | id), weights = ~ (1 | item),
#'               data = hsced)
#' summary(mod)
#'
#' # Generalized additive mixed model with factor structures -------------------
#'
#' # The cognition dataset contains simulated measurements of three latent
#' # time-dependent processes, corresponding to individuals' abilities in
#' # cognitive domains. We focus here on the first domain, and take a single
#' # random timepoint per person:
#' dat <- subset(cognition, domain == 1)
#' dat <- split(dat, f = dat$id)
#' dat <- lapply(dat, function(x) x[x$timepoint %in% sample(x$timepoint, 1), ])
#' dat <- do.call(rbind, dat)
#' dat$item <- factor(dat$item)
#'
#' # At each timepoint there are three items measuring ability in the cognitive
#' # domain. We fix the factor loading for the first measurement to one, and
#' # estimate the remaining two. This is specified in the loading matrix.
#' loading_matrix <- matrix(c(1, NA, NA), ncol = 1)
#'
#' # We can now estimate the model.
#' mod <- galamm(
#'   formula = y ~ 0 + item + sl(x, load.var = "loading") +
#'     (0 + loading | id),
#'   data = dat,
#'   load.var = "item",
#'   lambda = list(loading_matrix),
#'   factor = list("loading")
#' )
#'
#' # We can plot the estimated smooth term
#' plot_smooth(mod, shade = TRUE)
#'
#' @family modeling functions
#'
#' @md
galamm <- function(formula, weights = NULL, data, family = gaussian,
                   family_mapping = rep(1L, nrow(data)),
                   load.var = NULL, lambda = NULL, factor = NULL,
                   start = NULL, control = galamm_control()) {
  data <- stats::na.omit(data)
  if (nrow(data) == 0) stop("No data, nothing to do.")
  data <- as.data.frame(data)
  mc <- match.call()

  family_list <- setup_family(family)
  stopifnot(length(family_list) == length(unique(family_mapping)))

  tmp <- setup_factor(load.var, lambda, factor, data)
  data <- tmp$data
  lambda <- tmp$lambda
  rm(tmp)

  rf <- lme4::findbars(formula)
  rf <- if (!is.null(rf)) {
    stats::as.formula(paste("~", paste("(", rf, ")", collapse = "+")))
  }
  gobj <- gamm4(fixed = lme4::nobars(formula), random = rf, data = data)
  colnames(gobj$lmod$X) <- gsub("^X", "", colnames(gobj$lmod$X))

  response_obj <-
    setup_response_object(family_list, family_mapping, data, gobj)
  lambda_mappings <-
    define_factor_mappings(gobj, load.var, lambda, factor, data)

  theta_mapping <- gobj$lmod$reTrms$Lind - 1L
  theta_inds <- seq_along(gobj$lmod$reTrms$theta)
  beta_inds <- max(theta_inds) + seq_along(colnames(gobj$lmod$X))
  lambda_inds <- max(beta_inds) + seq_along(lambda[[1]][lambda[[1]] >= 2])

  if (!is.null(weights)) {
    weights_obj <- lme4::mkReTrms(lme4::findbars(weights), fr = data)
    if (length(weights_obj$flist) > 1) {
      stop("Multiple grouping terms in weights not yet implemented.")
    }
    weights_mapping <- as.integer(weights_obj$flist[[1]]) - 2L
    weights_inds <- length(unique(weights_mapping)) +
      max(c(theta_inds, beta_inds, lambda_inds)) - 1L
  } else {
    weights_obj <- NULL
    weights_mapping <- integer()
    weights_inds <- integer()
  }

  bounds <- c(
    gobj$lmod$reTrms$lower,
    rep(-Inf, length(beta_inds) + length(lambda_inds)),
    rep(0, length(weights_inds))
  )

  y <- response_obj[, 1]
  trials <- response_obj[, 2]
  u_init <- rep(0, nrow(gobj$lmod$reTrms$Zt))
  family_txt <- vapply(family_list, function(f) f$family, "a")
  k <- find_k(family_txt, family_mapping, y, trials)

  maxit_conditional_modes <- ifelse(
    length(family_list) == 1 & family_list[[1]]$family == "gaussian",
    1, control$maxit_conditional_modes
  )

  mlwrapper <- function(par, gradient = FALSE, hessian = FALSE) {
    marginal_likelihood_cpp(
      y = y,
      trials = trials,
      X = gobj$lmod$X,
      Zt = gobj$lmod$reTrms$Zt,
      Lambdat = gobj$lmod$reTrms$Lambdat,
      beta = par[beta_inds],
      theta = par[theta_inds],
      theta_mapping = theta_mapping,
      u_init = u_init,
      lambda = par[lambda_inds],
      lambda_mapping_X = lambda_mappings$lambda_mapping_X,
      lambda_mapping_X_covs = integer(),
      lambda_mapping_Zt = lambda_mappings$lambda_mapping_Zt,
      lambda_mapping_Zt_covs = integer(),
      weights = par[weights_inds],
      weights_mapping = weights_mapping,
      family = family_txt,
      family_mapping = as.integer(family_mapping) - 1L,
      k = k,
      maxit_conditional_modes = maxit_conditional_modes,
      gradient = gradient,
      hessian = hessian,
      epsilon_u = 1e-10,
      reduced_hessian = control$reduced_hessian
    )
  }

  mlmem <- memoise::memoise(mlwrapper)
  fn <- function(par, gradient, hessian = FALSE) {
    mlmem(par, gradient, hessian)$logLik
  }
  gr <- function(par, gradient, hessian = FALSE) {
    mlmem(par, gradient, hessian)$gradient
  }

  par_init <-
    set_initial_values(gobj, start, beta_inds, lambda_inds, weights_inds)

  if (control$method == "L-BFGS-B") {
    opt <- stats::optim(par_init,
      fn = fn, gr = gr, gradient = TRUE,
      method = "L-BFGS-B", lower = bounds,
      control = control$optim_control
    )
  } else {
    opt <- lme4::Nelder_Mead(
      fn = function(x) -fn(x, gradient = FALSE), par = par_init,
      lower = bounds, control = control$optim_control
    )
    opt$value <- -opt$fval
  }

  final_model <- mlwrapper(opt$par, gradient = TRUE, hessian = TRUE)

  # Update Cholesky factor of covariance matrix
  gobj$lmod$reTrms$Lambdat@x <- opt$par[theta_inds][gobj$lmod$reTrms$Lind]
  # Update Zt to include factor loadings (if there are factor loadings)
  if (length(lambda_inds) > 1) {
    gobj$lmod$reTrms$Zt@x <-
      c(1, opt$par[lambda_inds])[lambda_mappings$lambda_mapping_Zt + 2L]
  }

  # Random effects in original parametrization
  b <- Matrix::t(gobj$lmod$reTrms$Lambdat) %*% final_model$u

  # Compute prediction
  preds <- vapply(family_list, function(fam) {
    fam$linkinv(
      as.numeric(gobj$lmod$X %*% opt$par[beta_inds] +
        Matrix::t(gobj$lmod$reTrms$Zt) %*% b)
    )
  }, numeric(nrow(gobj$lmod$X)))

  fit <- unlist(Map(function(i, j) {
    preds[i, j]
  }, i = seq_len(nrow(preds)), j = family_mapping))

  pearson_residuals <- (response_obj[, 1] - fit) /
    unlist(Map(function(x, y) sqrt(family_list[[x]]$variance(y)),
      x = family_mapping, y = fit
    ))

  if (length(family_list) == 1 && family_list[[1]]$family == "gaussian") {
    deviance_residuals <- response_obj[, 1] - fit
    deviance <- -2 * opt$value
  } else {
    # McCullagh and Nelder (1989), page 39
    tmp <- lapply(
      family_list,
      function(x) {
        x$dev.resids(
          response_obj[, 1] / response_obj[, 2],
          fit, response_obj[, 2]
        )
      }
    )
    dev_res <- sqrt(vapply(
      seq_along(family_mapping),
      function(i) tmp[[family_mapping[[i]]]][[i]], 1
    ))

    deviance_residuals <- sign(response_obj[, 1] - fit) * dev_res
    deviance <- sum((dev_res)^2)
  }

  ret <- list()

  ret$call <- mc
  ret$random_effects <- list(
    b = as.numeric(b),
    u = as.numeric(final_model$u)
  )
  ret$model <- list(
    deviance = deviance,
    deviance_residuals = deviance_residuals,
    df = length(opt$par) +
      sum(vapply(family_list, function(x) is.na(x$dispersion), logical(1))),
    family = family_list,
    fit = fit,
    hessian = final_model$hessian,
    lmod = gobj$lmod,
    loglik = opt$value,
    n = nrow(gobj$lmod$X),
    pearson_residuals = pearson_residuals,
    reduced_hessian = control$reduced_hessian,
    response = response_obj[, 1],
    weights_obj = weights_obj
  )

  ret$parameters <- list(
    beta_inds = beta_inds,
    dispersion_parameter = final_model$phi,
    lambda_dummy = lambda,
    lambda_inds = lambda_inds,
    parameter_estimates = opt$par,
    parameter_names = c(
      paste0("theta", seq_along(theta_inds), recycle0 = TRUE),
      colnames(gobj$lmod$X),
      paste0("lambda", seq_along(lambda_inds), recycle0 = TRUE),
      paste0("weights", seq_along(weights_inds), recycle0 = TRUE)
    ),
    theta_inds = theta_inds,
    weights_inds = weights_inds
  )

  class(ret) <- "galamm"

  ret$gam <- gamm4.wrapup(gobj, ret, final_model)


  ret
}
