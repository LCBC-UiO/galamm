#' Fit a generalized additive latent and mixed model
#'
#' @param formula A formula
#' @param data A dataset
#' @param family Family
#' @param load.var Variable the factors load onto
#' @param lambda Loading
#' @param factor list of factors
#'
#' @return A model object
#' @export
#'
#' @importFrom stats gaussian
galamm <- function(formula, data, family = gaussian,
                   load.var = NULL, lambda = NULL, factor = NULL){

  mc <- match.call()
  if (is.character(family))
    family <- get(family, mode = "function", envir = parent.frame(2))
  if (is.function(family))
    family <- family()

  parameter_index <- 2
  for(i in seq_along(factor)){
    lambda[[i]][is.na(lambda[[i]])] <- seq(from = parameter_index, length.out = sum(is.na(lambda[[i]])))
    colnames(lambda[[i]]) <- factor[[i]]
    if(any(factor[[i]] %in% colnames(data))) stop("Factor already a column in data.")
    for(j in seq_along(factor[[i]])) {
      eval(parse(text = paste("data$", factor[[i]][[j]], "<-1")))
      rows_to_zero <- data[, load.var] %in% levels(data[, load.var])[lambda[[i]][, j] == 0]
      eval(parse(text = paste("data$", factor[[i]][[j]], "[rows_to_zero] <- 0")))
    }
    parameter_index <- max(lambda[[i]]) + 1
  }

  lmod <- lme4::lFormula(formula = formula, data = data, REML = FALSE)

  vars_in_fixed <- all.vars(lme4::nobars(formula)[-2])
  factor_in_fixed <- any(vapply(factor, function(x) any(x %in% vars_in_fixed), TRUE))
  vars_in_random <- do.call(c, lapply(lme4::findbars(formula), all.vars))
  factor_in_random <- any(vapply(factor, function(x) any(x %in% vars_in_random), TRUE))

  X <- lmod$X
  if(factor_in_fixed){
    lambda_mapping_X <- rep(-1L, length(X))

    stop("Not implemented yet")
  }
  Zt <- lmod$reTrms$Zt
  if(factor_in_random){

    mappings <- lapply(seq_along(lmod$reTrms$cnms), function(i){
      delta <- diff(lmod$reTrms$Ztlist[[i]]@p)
      cnms <- lmod$reTrms$cnms[[i]]

      if(any(cnms %in% colnames(lambda[[1]]))){
        ll <- lambda[[1]][, cnms, drop = FALSE] - 2L
      } else {
        return(rep(-1L, sum(delta)))
      }

      mapping_component <- rep(NA_integer_, sum(delta))
      for(j in seq_along(cnms)){
        cn <- cnms[[j]]
        inds <- which(data[, cn] != 0)
        mapping_component[inds] <- unlist(Map(function(x, y) rep(ll[x, cn], each = y),
                     x = data[inds, load.var], y = delta[inds]))

      }

      mapping_component
    })

    max_map <- max(vapply(mappings, length, 1))
    mappings <- lapply(mappings, function(x){
      if(length(x) < max_map){
        x <- c(x, rep(NA_real_, max_map - length(x)))
      } else {
        x
      }
    })
    lambda_mapping_Zt <- as.numeric(do.call(rbind, mappings))
    lambda_mapping_Zt <- lambda_mapping_Zt[!is.na(lambda_mapping_Zt)]

    stopifnot(length(lambda_mapping_Zt) == sum(diff(Zt@p)))
  }

  Lambdat <- lmod$reTrms$Lambdat
  theta_mapping <- lmod$reTrms$Lind - 1L

  theta_inds <- seq_along(lmod$reTrms$theta)
  beta_inds <- max(theta_inds) + seq_along(colnames(X))
  lambda_inds <- max(beta_inds) + seq_along(lambda[[1]][lambda[[1]] >= 2])
  bounds <- c(lmod$reTrms$lower, rep(-Inf, length(beta_inds) + length(lambda_inds)))
  response <- data[[all.vars(formula)[[1]]]]

  mlwrapper <- function(par, hessian = FALSE){
    marginal_likelihood(
      y = response,
      trials = rep(1, nrow(data)),
      X = X,
      Zt = Zt,
      Lambdat = Lambdat,
      beta = par[beta_inds],
      theta = par[theta_inds],
      theta_mapping = theta_mapping,
      lambda = par[lambda_inds],
      lambda_mapping_X = integer(),
      lambda_mapping_Zt = lambda_mapping_Zt,
      weights = numeric(),
      weights_mapping = integer(),
      family = "gaussian",
      family_mapping = rep(0L, nrow(data)),
      maxit_conditional_modes = 1,
      hessian = hessian
    )
  }

  mlmem <- memoise::memoise(mlwrapper)
  fn <- function(par){
    mlmem(par)$logLik
  }
  gr <- function(par){
    mlmem(par)$gradient
  }

  par_init <- c(lmod$reTrms$theta, rep(0, length(beta_inds)),
                rep(1, length(lambda_inds)))

  opt <- stats::optim(par_init, fn = fn, gr = gr,
                      method = "L-BFGS-B", lower = bounds,
                      control = list(fnscale = -1, lmm = 20, trace = 3))


  final_model <- mlwrapper(opt$par, TRUE)
  S <- -solve(final_model$hessian)

  # Update Cholesky factor of covariance matrix
  Lambdat@x <- opt$par[theta_inds][lmod$reTrms$Lind]
  # Update Zt to include factor loadings
  Zt@x <- c(1, opt$par[lambda_inds])[lambda_mapping_Zt + 2L]
  # Compute prediction
  fit <- as.numeric(X %*% opt$par[beta_inds] + Matrix::t(Zt) %*% Matrix::t(Lambdat) %*% final_model$u)

  ret <- list()
  ret$lambda <- lambda
  ret$cnms <- lmod$reTrms$cnms
  ret$fixef_names <- colnames(X)
  ret$vcov <- S
  ret$par <- opt$par
  ret$lambda_inds <- lambda_inds
  ret$beta_inds <- beta_inds
  ret$theta_inds <- theta_inds
  ret$phi <- final_model$phi
  ret$loglik <- opt$value

  ret$lmod <- lmod
  ret$mc <- mc
  ret$family <- family
  ret$df <- length(opt$par) + 1L

  ret$n <- nrow(X)
  ret$residuals <- response - fit

  class(ret) <- "galamm"

  ret
}
