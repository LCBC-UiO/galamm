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


test_that("Latent-observed interaction with smooths", {
  data <- latent_covariates
  data$m1 <- as.numeric(data$type == "measurement1")
  data$m2 <- as.numeric(data$type == "measurement2")
  formula <- y ~ 0 + m1 + m2 + s(x, k = 4, by = response) +
    (0 + loading | id)

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

  expect_equal(deviance(mod), 120.311, tolerance = 1e-3)
  expect_equal(mod$gam$edf,
    c(
      m1 = 0.999999999999793, m2 = 1.00000000000022,
      `s(x):response.1` = 1.76038497918713e-10,
      `s(x):response.2` = 2.52273958510005e-11,
      `s(x):response.3` = 1, `s(x):response.4` = 1
    ),
    tolerance = .1
  )

  data <- latent_covariates_long
  formula <- y ~ s(x, by = response) + (0 + loading | id) + (0 + response | id)

  mod <- galamm(
    formula = formula,
    data = data,
    load.var = load.var,
    lambda = lambda,
    factor = factor,
    factor_interactions = factor_interactions
  )

  expect_equal(deviance(mod), 130.6347, tolerance = 1e-3)
})
