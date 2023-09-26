test_that("Interaction between latent and observed covariates works", {
  formula <- y ~ type + x:response + (0 + loading | id)
  data <- latent_covariates
  load.var <- "type"
  lambda <- list(matrix(c(1, NA, NA), ncol = 1))
  factor <- list("loading")
  factor_interactions <- list(list(~1, ~1, ~x))

  mod <- galamm(
    formula = formula,
    data = data,
    load.var = load.var,
    lambda = lambda,
    factor = factor,
    factor_interactions = factor_interactions
  )

  expect_snapshot(print(summary(mod), digits = 2))

  mod0a <- galamm(
    formula = formula,
    data = data,
    load.var = load.var,
    lambda = lambda,
    factor = factor,
    factor_interactions = list(list(~1, ~1, ~1))
  )

  mod0b <- galamm(
    formula = formula,
    data = data,
    load.var = load.var,
    lambda = lambda,
    factor = factor
  )

  expect_equal(deviance(mod0a), deviance(mod0b))

  modq <- galamm(
    formula = formula,
    data = data,
    load.var = load.var,
    lambda = lambda,
    factor = factor,
    factor_interactions = list(list(~1, ~1, ~ x + I(x^2)))
  )

  expect_snapshot(print(summary(modq), digits = 2))
  expect_snapshot(print(anova(modq, mod), digits = 3))
})

test_that("Crossed latent-observed interaction models work", {
  formula <- y ~ type + x:response + (0 + loading | id) + (0 + response | id)
  data <- subset(latent_covariates_long, id < 100)
  load.var <- "type"
  lambda <- list(matrix(c(1, NA, NA), ncol = 1))
  factor <- list("loading")
  factor_interactions <- list(list(~1, ~1, ~x))

  mod <- galamm(
    formula = formula,
    data = data,
    load.var = load.var,
    lambda = lambda,
    factor = factor,
    factor_interactions = factor_interactions
  )

  expect_snapshot(print(summary(mod), digits = 2))

})

#
# test_that("Latent-observed interaction with smooths", {
#   data <- latent_covariates
#   data$m1 <- as.numeric(data$type == "measurement1")
#   data$m2 <- as.numeric(data$type == "measurement2")
#   formula <- y ~ 0 + m1 + m2 + s(x, k = 4, by = response) +
#     (0 + loading | id)
#   weights <- NULL
#
#   family <- gaussian
#   family_mapping <- rep(1L, nrow(data))
#   load.var <- "type"
#   lambda <- list(matrix(c(1, NA, NA), ncol = 1))
#   factor <- list("loading")
#   factor_interactions <- list(list(~1, ~1, ~x))
#   start <- NULL
#   control <- galamm_control()
#
#   mod <- galamm(
#     formula = formula,
#     data = data,
#     load.var = load.var,
#     lambda = lambda,
#     factor = factor,
#     factor_interactions = factor_interactions
#   )
#
#   expect_equal(deviance(mod), 1840.38, tolerance = 1e-3)
# })
#
