test_that("Mixed response works", {
  dat <- subset(mresp, id < 100)
  mod <- galamm(
    formula = y ~ x + (0 + loading | id),
    data = dat,
    family = c(gaussian, binomial),
    family_mapping = ifelse(dat$itemgroup == "a", 1L, 2L),
    load.var = "itemgroup",
    lambda = list(matrix(c(1, NA), ncol = 1)),
    factor = list("loading")
  )

  expect_equal(logLik(mod), structure(-441.199885684125, nobs = 396L, df = 5L,
                                      class = "logLik"))
  expect_equal(
    llikAIC(mod),
    c(
      AIC = 892.399771368249, BIC = 912.306842424522,
      logLik = -441.199885684125,
      deviance = 322.712453540071, df.resid = 391
    )
  )
  expect_equal(
    factor_loadings(mod),
    structure(c(1, 0.975278292857391, NA, 0.284026767903101), dim = c(
      2L,
      2L
    ), dimnames = list(c("lambda1", "lambda2"), c("loading", "SE")))
  )

  expect_equal(
    residuals(mod)[c(4, 8, 11)],
    c(0.647057715636119, 1.00640920636177, -1.0876543193226)
  )

  # Now test using initial values
  mod2 <- galamm(
    formula = y ~ x + (0 + loading | id),
    data = dat,
    family = c(gaussian, binomial),
    family_mapping = ifelse(dat$itemgroup == "a", 1L, 2L),
    load.var = "itemgroup",
    lambda = list(matrix(c(1, NA), ncol = 1)),
    factor = list("loading"),
    start = list(
      theta = mod$parameters$parameter_estimates[mod$parameters$theta_inds],
      beta = mod$parameters$parameter_estimates[mod$parameters$beta_inds],
      lambda = mod$parameters$parameter_estimates[mod$parameters$lambda_inds]
    )
  )
  expect_equal(logLik(mod2), logLik(mod))
})

test_that("Covariate measurement error model works", {
  lam <- matrix(c(1, 1, NA), ncol = 1)
  formula <- y ~ 0 + chd + (age * bus):chd + fiber +
    (age * bus):fiber + fiber2 + (0 + loading | id)

  mod <- galamm(
    formula = formula,
    data = diet,
    family = c(gaussian, binomial),
    family_mapping = ifelse(diet$item == "chd", 2L, 1L),
    factor = list("loading"),
    load.var = "item",
    lambda = list(lam),
    start = list(theta = 10)
  )

  expect_equal(mod$model$loglik, -1372.16038649521)

  expect_equal(
    factor_loadings(mod),
    structure(c(1, 1, -0.13392200444942, NA, NA, 0.0512082698234306),
              dim = 3:2, dimnames = list(c("lambda1", "lambda2", "lambda3"),
                                         c("loading", "SE"))),
    tolerance = 1e-4
  )

  formula0 <- y ~ 0 + chd + fiber + (age * bus):fiber + fiber2 +
    (0 + loading | id)

  mod0 <- galamm(
    formula = formula0,
    data = diet,
    family = c(gaussian, binomial),
    family_mapping = ifelse(diet$item == "chd", 2L, 1L),
    factor = list("loading"),
    load.var = "item",
    lambda = list(lam),
    start = list(theta = 10)
  )

  expect_snapshot(anova(mod, mod0))
})
