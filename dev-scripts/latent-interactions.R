rm(list = ls())
devtools::load_all()

load.var <- "type"
lambda <- list(matrix(c(1, NA, NA)))
factor <- list("loading")
factor_interactions <- list(list(~ 1, ~ 1, ~ x))

formula <- y ~ s(x, by = response) + (0 + loading | id) + (0 + response | id)
weights <- NULL
data <- latent_covariates_long
family <- gaussian
family_mapping = rep(1L, nrow(data))
start = NULL
control = galamm_control()





