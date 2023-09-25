rm(list = ls())
devtools::load_all()

load.var <- "type"
lambda <- list(matrix(c(1, NA, NA)))
factor <- list("loading")
factor_interactions <- list(list(~ 1, ~ 1, ~ x))

formula <- y ~ x:response + (0 + loading | id)
weights <- NULL
data <- latent_covariates
family <- gaussian
family_mapping = rep(1L, nrow(data))
start = NULL
control = galamm_control()

mod <- galamm(
  formula = formula,
  data = latent_covariates,
  load.var = load.var,
  lambda = lambda,
  factor = factor
)
