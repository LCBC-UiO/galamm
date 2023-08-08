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

  for(f in factor){
    for(l in f){
      if(l %in% colnames(data)) stop(paste("Name", l, "is already a column"))
      eval(parse(text = paste("data$", l, "<-1")))
    }
  }

  lmod <- lme4::lFormula(formula = formula, data = data, REML = FALSE)

  factor_in_fixed <- FALSE
  factor_in_random <- FALSE
  for(f in factor){
    factor_in_fixed <- factor_in_fixed || any(colnames(lmod$X) %in% f)
    factor_in_random <- factor_in_random || any(lmod$reTrms$cnms %in% f)
  }
  X <- lmod$X
  if(factor_in_fixed){
    stop("Not implemented yet")
  }
  Zt <- lmod$reTrms$Zt
  if(factor_in_random){
    delta <- as.integer(names(table(diff(Zt@p))))
    stopifnot(length(delta) == 1)
    # Subtract one for zero indexing in C++ and another one for the reference case
    lambda_mapping_Zt <- rep(as.integer(as.factor(data$item)), each = delta) - 2L
  }

  Lambdat <- lmod$reTrms$Lambdat
  theta_mapping <- lmod$reTrms$Lind - 1L

  theta_inds <- seq_along(lmod$reTrms$theta)
  beta_inds <- max(theta_inds) + seq_along(colnames(X))
  lambda_inds <- max(beta_inds) + seq_along(lambda[[1]][is.na(lambda[[1]])])
  bounds <- c(lmod$reTrms$lower, rep(-Inf, length(beta_inds) + length(lambda_inds)))

  mlwrapper <- function(par, hessian = FALSE){
    marginal_likelihood(
      y = data$y,
      trials = rep(1, length(data$y)),
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
                      control = list(fnscale = -1, lmm = 20))

  final_model <- mlwrapper(opt$par, TRUE)
  S <- -solve(final_model$hessian)

  # Update Cholesky factor of covariance matrix
  Lambdat@x <- opt$par[theta_inds][lmod$reTrms$Lind]
  # Update Zt to include factor loadings
  Zt@x <- c(1, opt$par[lambda_inds])[lambda_mapping_Zt + 2L]
  # Compute prediction
  fit <- as.numeric(X %*% opt$par[beta_inds] + Matrix::t(Zt) %*% Matrix::t(Lambdat) %*% final_model$u)

  ret <- list()
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
  ret$residuals <- data$y - fit

  class(ret) <- "galamm"

  ret
}
