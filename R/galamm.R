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
#'   introduction. Random effects are specifified using \code{lme4} syntax,
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
#' @return A model object of class \code{galamm}.
#' @export
#'
#' @references \insertAllCited{}
#'
#'
galamm <- function(formula, weights = NULL, data, family = gaussian,
                   family_mapping = rep(1L, nrow(data)),
                   load.var = NULL, lambda = NULL, factor = NULL,
                   start = NULL, control = galamm_control()) {
  data <- as.data.frame(data)
  mc <- match.call()

  family_list <- setup_family(family)
  stopifnot(length(family_list) == length(unique(family_mapping)))

  tmp <- setup_factor(load.var, lambda, factor, data)
  data <- tmp$data
  lambda <- tmp$lambda
  rm(tmp)

  rf <- lme4::findbars(formula)
  rf <- if (!is.null(rf)) as.formula(paste("~", paste("(", rf, ")", collapse = "+")))
  gobj <- gamm4(fixed = lme4::nobars(formula), random = rf, data = data)

  colnames(gobj$lmod$X) <- gsub("^X", "", colnames(gobj$lmod$X))

  response_obj <- setup_response_object(family_list, family_mapping, data, gobj)
  lambda_mappings <- define_factor_mappings(gobj, load.var, lambda, factor, data)

  theta_mapping <- gobj$lmod$reTrms$Lind - 1L

  theta_inds <- seq_along(gobj$lmod$reTrms$theta)
  beta_inds <- max(theta_inds) + seq_along(colnames(gobj$lmod$X))
  lambda_inds <- max(beta_inds) + seq_along(lambda[[1]][lambda[[1]] >= 2])
  bounds <- c(
    gobj$lmod$reTrms$lower,
    rep(-Inf, length(beta_inds) + length(lambda_inds))
  )

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

  y <- response_obj[, 1]
  trials <- response_obj[, 2]
  u_init <- rep(0, nrow(gobj$lmod$reTrms$Zt))

  family_txt <- vapply(family_list, function(f) f$family, "a")

  k <- find_k(family_txt, family_mapping, y, trials)

  maxit_conditional_modes <- ifelse(
    length(family_list) == 1 & family_list[[1]]$family == "gaussian",
    1, control$maxit_conditional_modes
  )

  mlwrapper <- function(par, hessian = FALSE) {
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
      gradient = TRUE,
      hessian = hessian,
      epsilon_u = 1e-10
    )
  }

  mlmem <- memoise::memoise(mlwrapper)
  fn <- function(par) {
    mlmem(par)$logLik
  }
  gr <- function(par) {
    mlmem(par)$gradient
  }

  par_init <- set_initial_values(gobj, start, beta_inds, lambda_inds, weights_inds)

  opt <- stats::optim(par_init,
    fn = fn, gr = gr,
    method = "L-BFGS-B", lower = bounds,
    control = control$optim_control
  )

  final_model <- mlwrapper(opt$par, TRUE)

  # Update Cholesky factor of covariance matrix
  gobj$lmod$reTrms$Lambdat@x <- opt$par[theta_inds][gobj$lmod$reTrms$Lind]
  # Update Zt to include factor loadings (if there are factor loadings)
  if (length(lambda_inds) > 1) {
    gobj$lmod$reTrms$Zt@x <- c(1, opt$par[lambda_inds])[lambda_mappings$lambda_mapping_Zt + 2L]
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

  ret <- list()
  ret$b <- b
  ret$u <- final_model$u
  ret$lambda <- lambda
  ret$cnms <- gobj$lmod$reTrms$cnms
  ret$par_names <- c(
    paste0("theta", seq_along(theta_inds), recycle0 = TRUE),
    colnames(gobj$lmod$X),
    paste0("lambda", seq_along(lambda_inds), recycle0 = TRUE),
    paste0("weights", seq_along(weights_inds), recycle0 = TRUE)
  )
  ret$hessian <- final_model$hessian
  ret$par <- opt$par
  ret$lambda_inds <- lambda_inds
  ret$beta_inds <- beta_inds
  ret$theta_inds <- theta_inds
  ret$weights_inds <- weights_inds
  ret$phi <- final_model$phi
  ret$loglik <- opt$value

  ret$lmod <- gobj$lmod
  ret$weights_obj <- weights_obj
  ret$call <- mc
  ret$family <- family_list
  ret$df <- length(opt$par) +
    sum(vapply(family_list, function(x) is.na(x$dispersion), logical(1)))

  ret$n <- nrow(gobj$lmod$X)

  ret$pearson_residuals <- (response_obj[, 1] - fit) /
    unlist(Map(function(x, y) sqrt(family_list[[x]]$variance(y)),
      x = family_mapping, y = fit
    ))

  if (length(family_list) == 1 && family_list[[1]]$family == "gaussian") {
    ret$deviance_residuals <- response_obj[, 1] - fit
    ret$deviance <- -2 * ret$loglik
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

    ret$deviance_residuals <- sign(response_obj[, 1] - fit) * dev_res
    ret$deviance <- sum((dev_res)^2)
  }

  ret$fit <- fit
  ret$response <- response_obj[, 1]

  class(ret) <- "galamm"

  ## Deal with smooth terms ----
  # If there are smooth terms in the model, postprocess them
  # This should eventually be a function, and it should be specified that this
  # code is derived from gamm4, with author Simon Wood
  if (length(gobj$G$smooth) > 0) {

    ret$gam <- gamm4.wrapup(gobj, ret, final_model)
  }


  ret
}
