test_that("Interaction between latent and observed covariates works", {
  formula <- y ~ type + x:response + (0 + loading | id)
  data <- latent_covariates
  load.var <- "type"
  lambda <- list(matrix(c(1, NA, NA), ncol = 1))
  factor <- list("loading")
  factor_interactions <- list(list(~ 1, ~ 1, ~ x))

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
    factor_interactions = list(list(~ 1, ~ 1, ~ 1))
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
    factor_interactions = list(list(~ 1, ~ 1, ~ x + I(x^2)))
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
  factor_interactions <- list(list(~ 1, ~ 1, ~ x))

  mod <- galamm(
    formula = formula,
    data = data,
    load.var = load.var,
    lambda = lambda,
    factor = factor,
    factor_interactions = factor_interactions,
    start = list(theta = c(1.084, 1.46),
                 beta = c(0.205, -0.013, -0.243, 0.496),
                 lambda = c(2.176, 0.674, -0.014), weights = numeric(0)),
    control = galamm_control(optim_control = list(maxit = 0))
  )

  expect_snapshot(print(summary(mod), digits = 2))


})
