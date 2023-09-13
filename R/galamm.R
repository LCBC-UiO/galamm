#' Fit a generalized additive latent and mixed model
#'
#' This function fits a generalized additive latent and mixed model (GALAMMs),
#' as described in
#' \insertCite{sorensenLongitudinalModelingAgeDependent2023;textual}{galamm}.
#' The building blocks of these models are generalized additive mixed models
#' (GAMMs) \insertCite{woodGeneralizedAdditiveModels2017a}{galamm}, of which
#' generalized linear mixed models
#' \insertCite{breslowApproximateInferenceGeneralized1993,hendersonBestLinearUnbiased1975,lairdRandomEffectsModelsLongitudinal1982}{galamm}
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
  stopifnot(length(family) == length(unique(family_mapping)))

  data <- as.data.frame(data)

  mc <- match.call()

  if (!is.list(family)) family <- list(family)
  family_list <- lapply(family, function(f) {
    if (is.character(f)) {
      get(f, mode = "function", envir = parent.frame(2))
    }
    if (is.function(f)) {
      f()
    }
  })


  parameter_index <- 2
  if (!is.null(factor)) {
    for (i in seq_along(factor)) {
      lambda[[i]][is.na(lambda[[i]])] <-
        seq(from = parameter_index, length.out = sum(is.na(lambda[[i]])))
      colnames(lambda[[i]]) <- factor[[i]]

      if (any(factor[[i]] %in% colnames(data))) {
        stop("Factor already a column in data.")
      }
      for (j in seq_along(factor[[i]])) {
        if (length(unique(data[, load.var])) != length(lambda[[i]][, j])) {
          stop("lambda matrix must contain one row for each element in load.var")
        }
        eval(parse(text = paste("data$", factor[[i]][[j]], "<-1")))
        rows_to_zero <-
          data[, load.var] %in% levels(data[, load.var])[lambda[[i]][, j] == 0]
        eval(
          parse(
            text =
              paste("data$", factor[[i]][[j]], "[rows_to_zero] <- 0")
          )
        )
      }
      parameter_index <- max(lambda[[i]]) + 1
    }
  }

  rf <- lme4::findbars(formula)
  gobj <- gamm4(
    fixed = lme4::nobars(formula),
    random = if (!is.null(rf)) as.formula(paste("~", paste("(", rf, ")", collapse = "+"))),
    data = data
  )
  lmod <- gobj$lmod
  colnames(lmod$X) <- gsub("^X", "", colnames(lmod$X))

  response_obj <- matrix(nrow = nrow(lmod$X), ncol = 2)
  for (i in seq_along(family_list)) {
    f <- family_list[[i]]
    mf <- model.frame(lme4::nobars(gobj$fake.formula), data = data[family_mapping == i, ])
    mr <- model.response(mf)

    if (f$family == "binomial" && !is.null(dim(mr))) {
      trials <- rowSums(mr)
      response <- mr[, 1, drop = TRUE]
    } else {
      trials <- rep(1, sum(family_mapping == i))
      response <- mr
    }
    response_obj[family_mapping == i, ] <-
      cbind(response = response, trials = trials)
  }
  rm(trials, response)

  vars_in_fixed <- all.vars(gobj$fake.formula[-2])
  factor_in_fixed <-
    vapply(factor, function(x) any(x %in% vars_in_fixed), TRUE)
  vars_in_random <- unique(unlist(lmod$reTrms$cnms))
  factor_in_random <-
    vapply(factor, function(x) any(vapply(vars_in_random, function(y) any(vapply(x, function(z) grepl(z, y), TRUE)), TRUE)), TRUE)

  X <- lmod$X
  if (any(factor_in_fixed)) {
    lambda_mapping_X <- rep(-1L, length(X))
  } else {
    lambda_mapping_X <- integer()
  }

  for (f in seq_along(factor_in_fixed)) {
    if (factor_in_fixed[[f]]) {
      cols <- grep(factor[[1]], colnames(X))
      for (cc in cols) {
        lambda_mapping_X[
          seq(from = (cc - 1) * nrow(X) + 1, to = cc * nrow(X))
        ] <-
          lambda[[1]][data[, load.var]] - 2L
      }
    }
  }

  Zt <- lmod$reTrms$Zt

  if (any(factor_in_random)) {
    lambda_mapping_Zt <- rep(-1L, sum(diff(Zt@p)))
  } else {
    lambda_mapping_Zt <- integer()
  }

  for (f in seq_along(factor_in_random)) {
    if (factor_in_random[[f]]) {
      mappings <- lapply(seq_along(lmod$reTrms$cnms), function(i) {
        delta <- diff(lmod$reTrms$Ztlist[[i]]@p)
        cnms <- lmod$reTrms$cnms[[i]]

        cnms_match <- vapply(
          colnames(lambda[[f]]),
          function(x) any(grepl(x, cnms)), TRUE
        )
        if (any(cnms_match)) {
          ll <- lambda[[f]][, names(cnms_match[cnms_match]), drop = FALSE] - 2L
        } else {
          return(rep(-1L, sum(delta)))
        }

        mapping_component <- rep(NA_integer_, length(delta))
        for (j in seq_along(cnms)) {
          cn <- unlist(lapply(factor[[f]], function(x) {
            m <- regexpr(x, cnms[[j]], fixed = TRUE)
            regmatches(cnms[[j]], m)
          }))

          inds <- which(data[, cn] != 0)
          inds_expanded <- unlist(Map(function(x, y) rep(x, each = y), x = inds, y = pmin(1, delta[inds])))

          mapping_component[inds_expanded] <-
            Map(function(x, y) rep(ll[x, cn], each = y),
              x = data[inds, load.var], y = delta[inds]
            )
        }

        mapping_component
      })

      lambda_mapping_Zt <- unlist(do.call(function(...) mapply(c, ..., SIMPLIFY = FALSE), mappings))
      lambda_mapping_Zt <- lambda_mapping_Zt[!is.na(lambda_mapping_Zt)]

      stopifnot(length(lambda_mapping_Zt) == sum(diff(Zt@p)))
    }
  }

  Lambdat <- lmod$reTrms$Lambdat
  theta_mapping <- lmod$reTrms$Lind - 1L

  theta_inds <- seq_along(lmod$reTrms$theta)
  beta_inds <- max(theta_inds) + seq_along(colnames(X))
  lambda_inds <- max(beta_inds) + seq_along(lambda[[1]][lambda[[1]] >= 2])
  bounds <- c(
    lmod$reTrms$lower,
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
  u_init <- rep(0, nrow(Zt))

  family_txt <- vapply(family_list, function(f) f$family, "a")

  k <- numeric(length(family_txt))
  for (i in seq_along(k)) {
    if (family_txt[[i]] == "gaussian") {
      k[[i]] <- 0
    } else if (family_txt[[i]] == "binomial") {
      trials0 <- trials[family_mapping == i]
      y0 <- y[family_mapping == i]
      k[[i]] <- sum(lgamma(trials0 + 1) - lgamma(y0 + 1) - lgamma(trials0 - y0 + 1))
    } else if (family_txt[[i]] == "poisson") {
      trials0 <- trials[family_mapping == i]
      y0 <- y[family_mapping == i]
      k[[i]] <- -sum(lgamma(y0 + 1))
    }
  }

  maxit_conditional_modes <- ifelse(
    length(family_list) == 1 & family_list[[1]]$family == "gaussian",
    1, control$maxit_conditional_modes
  )

  mlwrapper <- function(par, hessian = FALSE) {
    marginal_likelihood_cpp(
      y = y,
      trials = trials,
      X = X,
      Zt = Zt,
      Lambdat = Lambdat,
      beta = par[beta_inds],
      theta = par[theta_inds],
      theta_mapping = theta_mapping,
      u_init = u_init,
      lambda = par[lambda_inds],
      lambda_mapping_X = lambda_mapping_X,
      lambda_mapping_X_covs = integer(),
      lambda_mapping_Zt = lambda_mapping_Zt,
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

  theta_init <- if (!is.null(start$theta)) {
    start$theta
  } else {
    lmod$reTrms$theta
  }
  beta_init <- if (!is.null(start$beta)) {
    start$beta
  } else {
    rep(0, length(beta_inds))
  }
  lambda_init <- if (!is.null(start$lambda)) {
    start$lambda
  } else {
    rep(1, length(lambda_inds))
  }
  weights_init <- if (!is.null(start$weights)) {
    start$weights
  } else {
    rep(1, length(weights_inds))
  }
  par_init <- c(theta_init, beta_init, lambda_init, weights_init)

  opt <- stats::optim(par_init,
    fn = fn, gr = gr,
    method = "L-BFGS-B", lower = bounds,
    control = optim_control(control)
  )

  final_model <- mlwrapper(opt$par, TRUE)

  # Update Cholesky factor of covariance matrix
  Lambdat@x <- opt$par[theta_inds][lmod$reTrms$Lind]
  # Update Zt to include factor loadings (if there are factor loadings)
  if (length(lambda_inds) > 1) {
    Zt@x <- c(1, opt$par[lambda_inds])[lambda_mapping_Zt + 2L]
  }

  # Random effects in original parametrization
  b <- Matrix::t(Lambdat) %*% final_model$u

  # Compute prediction
  preds <- vapply(family_list, function(fam) {
    fam$linkinv(
      as.numeric(X %*% opt$par[beta_inds] +
        Matrix::t(Zt) %*% b)
    )
  }, numeric(nrow(X)))

  fit <- unlist(Map(function(i, j) {
    preds[i, j]
  }, i = seq_len(nrow(preds)), j = family_mapping))

  ret <- list()
  ret$b <- b
  ret$u <- final_model$u
  ret$lambda <- lambda
  ret$cnms <- lmod$reTrms$cnms
  ret$par_names <- c(
    paste0("theta", seq_along(theta_inds), recycle0 = TRUE),
    colnames(X),
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

  ret$lmod <- lmod
  ret$weights_obj <- weights_obj
  ret$call <- mc
  ret$family <- family
  ret$df <- length(opt$par) +
    sum(vapply(family_list, function(x) is.na(x$dispersion), logical(1)))

  ret$n <- nrow(X)

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
    object <- list(
      smooth = gobj$G$smooth,
      nsdf = gobj$G$nsdf,
      df.null = nrow(gobj$G$X),
      terms = gobj$gam.terms,
      pterms = gobj$G$pterms,
      xlevels = gobj$G$xlevels,
      contrasts = gobj$G$contrasts,
      assign = gobj$G$assign,
      cmX = gobj$G$cmX,
      var.summary = gobj$G$var.summary
    )

    pvars <- all.vars(delete.response(object$terms))
    object$pred.formula <- if (length(pvars) > 0) {
      reformulate(pvars)
    } else {
      NULL
    }

    B <- Matrix::Matrix(0, ncol(gobj$G$Xf), ncol(gobj$G$Xf))
    diag(B) <- 1
    Xfp <- gobj$G$Xf

    ## Transform  parameters back to the original space....
    bf <- as.numeric(fixef(ret)) ## the fixed effects
    br <- ranef(ret) ## a named list
    if (gobj$G$nsdf) p <- bf[1:gobj$G$nsdf] else p <- array(0, 0) ## fixed parametric componet
    if (gobj$G$m > 0) {
      for (i in 1:gobj$G$m) {
        fx <- gobj$G$smooth[[i]]$fixed
        first <- gobj$G$smooth[[i]]$first.f.para
        last <- gobj$G$smooth[[i]]$last.f.para
        if (first <= last) beta <- bf[first:last] else beta <- array(0, 0)
        if (fx) {
          b <- beta
        } else { ## not fixed so need to undo transform of random effects etc.
          b <- rep(0, 0)
          for (k in seq_along(gobj$G$smooth[[i]]$lmer.name)) { ## collect all coefs associated with this smooth
            b <- c(b, as.numeric(br[[gobj$G$smooth[[i]]$lmer.name[k]]][[1]]))
          }
          b <- b[gobj$G$smooth[[i]]$rind] ## make sure coefs are in order expected by smooth
          b <- c(b, beta)
          b <- gobj$G$smooth[[i]]$trans.D * b
          if (!is.null(gobj$G$smooth[[i]]$trans.U)) b <- gobj$G$smooth[[i]]$trans.U %*% b ## transform back to original
        }
        p <- c(p, b)

        ## now fill in B...
        ind <- gobj$G$smooth[[i]]$first.para:gobj$G$smooth[[i]]$last.para
        if (!fx) {
          D <- gobj$G$smooth[[i]]$trans.D
          if (is.null(gobj$G$smooth[[i]]$trans.U)) {
            B[ind, ind] <- Matrix::Diagonal(length(D), D)
          } else {
            B[ind, ind] <- t(D * t(gobj$G$smooth[[i]]$trans.U))
          }
        }
        ## and finally transform G$Xf into fitting parameterization...
        Xfp[, ind] <- gobj$G$Xf[, ind, drop = FALSE] %*% B[ind, ind, drop = FALSE]
      }
    }

    object$coefficients <- p

    # At this point I have to implement a VarCorr.galamm method
  }





  ret
}
