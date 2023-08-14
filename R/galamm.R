#' Fit a generalized additive latent and mixed model
#'
#' @param formula A formula
#' @param weights An optional formula object specifying an expression for the
#'   residual variance. Defaults to \code{NULL}, corresponding to homoscedastic
#'   errors.
#' @param data A dataset
#' @param family A vector of families
#' @param family_mapping A vector mapping from the elements of "family" to rows
#'   of "data". Defaults to \code{rep(1L, nrow(data))}, which means that all
#'   observations are distributed according to the first element of "family".
#' @param load.var Variable the factors load onto
#' @param lambda Loading
#' @param factor list of factors
#' @param start A named list of starting values for parameters. Possible names
#'   of list elements are "theta", "beta", and "lambda", all of which represent
#'   numerical vectors.
#'
#' @return A model object
#' @export
#'
#' @importFrom stats gaussian model.frame model.response
#' @importFrom Rdpack reprompt
galamm <- function(formula, weights = NULL, data, family = gaussian,
                   family_mapping = rep(1L, nrow(data)),
                   load.var = NULL, lambda = NULL, factor = NULL,
                   start = NULL) {
  stopifnot(length(family) == length(unique(family_mapping)))
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
      lambda[[i]][is.na(lambda[[i]])] <- seq(from = parameter_index, length.out = sum(is.na(lambda[[i]])))
      colnames(lambda[[i]]) <- factor[[i]]
      if (any(factor[[i]] %in% colnames(data))) stop("Factor already a column in data.")
      for (j in seq_along(factor[[i]])) {
        eval(parse(text = paste("data$", factor[[i]][[j]], "<-1")))
        rows_to_zero <- data[, load.var] %in% levels(data[, load.var])[lambda[[i]][, j] == 0]
        eval(parse(text = paste("data$", factor[[i]][[j]], "[rows_to_zero] <- 0")))
      }
      parameter_index <- max(lambda[[i]]) + 1
    }
  }

  lmod <- lme4::lFormula(formula = formula, data = data, REML = FALSE)

  response_obj <- matrix(nrow = nrow(lmod$X), ncol = 2)
  for (i in seq_along(family_list)) {
    f <- family_list[[i]]
    mf <- model.frame(nobars(formula), data = data[family_mapping == i, ])
    mr <- model.response(mf)

    if (f$family == "binomial" && !is.null(dim(mr))) {
      trials <- rowSums(mr)
      response <- mr[, 1, drop = TRUE]
    } else {
      trials <- rep(1, sum(family_mapping == i))
      response <- mr
    }
    response_obj[family_mapping == i, ] <- cbind(response = response, trials = trials)
  }
  rm(trials, response)

  vars_in_fixed <- all.vars(lme4::nobars(formula)[-2])
  factor_in_fixed <- vapply(factor, function(x) any(x %in% vars_in_fixed), TRUE)
  vars_in_random <- do.call(c, lapply(lme4::findbars(formula), all.vars))
  factor_in_random <- vapply(factor, function(x) any(x %in% vars_in_random), TRUE)

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
        lambda_mapping_X[seq(from = (cc - 1) * nrow(X) + 1, to = cc * nrow(X))] <-
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

        cnms_match <- vapply(colnames(lambda[[f]]), function(x) any(grepl(x, cnms)), TRUE)
        if (any(cnms_match)) {
          ll <- lambda[[f]][, names(cnms_match[cnms_match]), drop = FALSE] - 2L
        } else {
          return(rep(-1L, sum(delta)))
        }

        mapping_component <- rep(NA_integer_, sum(delta))
        for (j in seq_along(cnms)) {
          cn <- cnms[[j]]
          inds <- which(data[, cn] != 0)
          mapping_component[inds] <- unlist(Map(function(x, y) rep(ll[x, cn], each = y),
            x = data[inds, load.var], y = delta[inds]
          ))
        }

        mapping_component
      })

      max_map <- max(vapply(mappings, length, 1))
      mappings <- lapply(mappings, function(x) {
        if (length(x) < max_map) {
          x <- c(x, rep(NA_real_, max_map - length(x)))
        } else {
          x
        }
      })
      lambda_mapping_Zt <- as.numeric(do.call(rbind, mappings))
      lambda_mapping_Zt <- lambda_mapping_Zt[!is.na(lambda_mapping_Zt)]

      stopifnot(length(lambda_mapping_Zt) == sum(diff(Zt@p)))
    }
  }

  Lambdat <- lmod$reTrms$Lambdat
  theta_mapping <- lmod$reTrms$Lind - 1L

  theta_inds <- seq_along(lmod$reTrms$theta)
  beta_inds <- max(theta_inds) + seq_along(colnames(X))
  lambda_inds <- max(beta_inds) + seq_along(lambda[[1]][lambda[[1]] >= 2])
  bounds <- c(lmod$reTrms$lower, rep(-Inf, length(beta_inds) + length(lambda_inds)))

  if (!is.null(weights)) {
    weights_obj <- lme4::mkReTrms(lme4::findbars(weights), fr = data)
    if (length(weights_obj$flist) > 1) {
      stop("Multiple grouping terms in weights not yet implemented.")
    }
    weights_mapping <- as.integer(weights_obj$flist[[1]]) - 2L
    weights_inds <- length(unique(weights_mapping)) + max(c(theta_inds, beta_inds, lambda_inds)) - 1L
  } else {
    weights_obj <- NULL
    weights_mapping <- integer()
    weights_inds <- integer()
  }


  mlwrapper <- function(par, hessian = FALSE) {
    marginal_likelihood(
      y = response_obj[, 1],
      trials = response_obj[, 2],
      X = X,
      Zt = Zt,
      Lambdat = Lambdat,
      beta = par[beta_inds],
      theta = par[theta_inds],
      theta_mapping = theta_mapping,
      lambda = par[lambda_inds],
      lambda_mapping_X = lambda_mapping_X,
      lambda_mapping_Zt = lambda_mapping_Zt,
      weights = par[weights_inds],
      weights_mapping = weights_mapping,
      family = vapply(family_list, function(f) f$family, "a"),
      family_mapping = as.integer(family_mapping) - 1L,
      maxit_conditional_modes = ifelse(length(family_list) == 1 & family_list[[1]]$family == "gaussian", 1, 10),
      hessian = hessian
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
    control = list(fnscale = -1, lmm = 20, trace = 3)
  )


  final_model <- mlwrapper(opt$par, TRUE)
  S <- tryCatch(
    {
      -solve(final_model$hessian)
    },
    error = function(e) {
      message("Hessian rank deficient. Could not compute covariance matrix.")
      NULL
    }
  )


  # Update Cholesky factor of covariance matrix
  Lambdat@x <- opt$par[theta_inds][lmod$reTrms$Lind]
  # Update Zt to include factor loadings (if there are factor loadings)
  if (length(lambda_inds) > 1) {
    Zt@x <- c(1, opt$par[lambda_inds])[lambda_mapping_Zt + 2L]
  }
  # Compute prediction
  preds <- vapply(family_list, function(fam) {
    fam$linkinv(
      as.numeric(X %*% opt$par[beta_inds] +
        Matrix::t(Zt) %*% Matrix::t(Lambdat) %*% final_model$u)
    )
  }, numeric(nrow(X)))

  fit <- unlist(Map(function(i, j) {
    preds[i, j]
  }, i = seq_len(nrow(preds)), j = family_mapping))


  ret <- list()
  ret$lambda <- lambda
  ret$cnms <- lmod$reTrms$cnms
  ret$fixef_names <- colnames(X)
  ret$vcov <- S
  ret$par <- opt$par
  ret$lambda_inds <- lambda_inds
  ret$beta_inds <- beta_inds
  ret$theta_inds <- theta_inds
  ret$weights_inds <- weights_inds
  ret$phi <- final_model$phi
  ret$loglik <- opt$value

  ret$lmod <- lmod
  ret$weights_obj <- weights_obj
  ret$mc <- mc
  ret$family <- family
  ret$df <- length(opt$par) + sum(vapply(family_list, function(x) is.na(x$dispersion), logical(1)))

  ret$n <- nrow(X)

  ret$pearson_residuals <- (response_obj[, 1] - fit) / unlist(Map(function(x, y) sqrt(family_list[[x]]$variance(y)),
    x = family_mapping, y = fit
  ))

  if (length(family_list) == 1 && family_list[[1]]$family == "gaussian") {
    ret$deviance_residuals <- response_obj[, 1] - fit
    ret$deviance <- -2 * ret$loglik
  } else {
    # McCullagh and Nelder (1989), page 39
    tmp <- lapply(family_list, function(x) x$dev.resids(response_obj[, 1] / response_obj[, 2], fit, response_obj[, 2]))
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

  ret
}
