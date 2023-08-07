#' Fit a generalized additive latent and mixed model
#'
#' @param formula A formula
#' @param data A dataset
#' @param load.var Variable the factors load onto
#' @param lambda Loading
#' @param factor list of factors
#'
#' @return A model object
#' @export
#'
galamm <- function(formula, data, load.var = NULL, lambda = NULL, factor = NULL){

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
    lambda_mapping_Zt <- rep(data$item, each = delta) - 2L
  }

  Lambdat <- lmod$reTrms$Lambdat
  theta_mapping <- lmod$reTrms$Lind - 1L

  theta_inds <- seq_along(lmod$reTrms$theta)
  beta_inds <- max(theta_inds) + seq_along(colnames(X))
  lambda_inds <- max(beta_inds) + seq_along(lambda[[1]][is.na(lambda[[1]])])
  bounds <- c(lmod$reTrms$lower, rep(-Inf, length(beta_inds) + length(lambda_inds)))

  mlwrapper <- function(par){
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
      maxit_conditional_modes = 1
    )
  }

  mlmem <- memoise::memoise(mlwrapper)
  fn <- function(par){
    mlmem(par)$logLik
  }
  gr <- function(par){
    mlmem(par)$gradient
  }

  par_init <- c(lmod$reTrms$theta, rep(0, length(beta_inds)), rep(1, length(lambda_inds)))
  opt <- optim(par_init, fn = fn, gr = gr,
               method = "L-BFGS-B", lower = bounds,
               control = list(fnscale = -1))

  opt
}
