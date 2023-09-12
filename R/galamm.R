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
#' @param control Control object. Result of calling
#'   \code{\link{galamm_control}}.
#'
#' @return A model object
#' @export
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

        mapping_component <- rep(NA_integer_, sum(delta))
        for (j in seq_along(cnms)) {
          cn <- unlist(lapply(factor[[f]], function(x) {
            m <- regexpr(x, cnms[[j]], fixed = TRUE)
            regmatches(cnms[[j]], m)
          }))

          inds <- which(data[, cn] != 0)
          inds_expanded <- unlist(Map(function(x, y) rep(x, each = y), x = inds, y = delta[inds]))
          if (any(delta[inds] > 1) && !any(delta[inds] == 0)) inds_expanded <- order(inds_expanded)
          mapping_component[inds_expanded] <-
            unlist(Map(function(x, y) rep(ll[x, cn], each = y),
              x = data[inds, load.var], y = delta[inds]
            ))
        }

        mapping_component
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
      maxit_conditional_modes =
        ifelse(
          length(family_list) == 1 & family_list[[1]]$family == "gaussian",
          1, control$maxit_conditional_modes
        ),
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
