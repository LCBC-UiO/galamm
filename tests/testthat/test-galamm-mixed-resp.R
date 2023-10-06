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

  expect_error(predict(mod, type = "response"),
               "For mixed response model, only type='link' works.")

  expect_equal(logLik(mod), structure(-441.199885684125,
    nobs = 396L, df = 5L,
    class = "logLik"
  ))
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

  expect_equal(
    tail(predict(mod)),
    c(0.579766252831168, 0.650224364469182, 0.467589361633112,
      0.104006021737622, 0.611398991366611, 0.619149621098952),
    tolerance = 1e-4)

  expect_equal(
    tail(fitted(mod)),
    c(0.564189106544179, 0.635617477751538, -0.38290827754476,
      -0.74649161744025, 0.401960973148028, 0.409856827991987),
    tolerance = 1e-4)

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

test_that("Mixed response works with multiple trials", {
  set.seed(100)
  dat <- subset(mresp, id < 100)
  dat$trials <- sample(5, nrow(dat), replace = TRUE)
  mod <- galamm(
    formula = cbind(y, trials - y) ~ x + (0 + loading | id),
    data = dat,
    family = c(gaussian, binomial),
    family_mapping = ifelse(dat$itemgroup == "a", 1L, 2L),
    load.var = "itemgroup",
    lambda = list(matrix(c(1, NA), ncol = 1)),
    factor = list("loading")
  )

  expect_equal(deviance(mod), 885.73410657623)
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
      dim = 3:2, dimnames = list(
        c("lambda1", "lambda2", "lambda3"),
        c("loading", "SE")
      )
    ),
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


test_that("Mixed response and heteroscedastic error works", {
  family_mapping <- ifelse(mresp$itemgroup == "a", 1L, 2L)
  mod <- galamm(
    formula = y ~ x + (1 | id),
    weights = ~ (0 + isgauss | grp),
    family = c(gaussian, binomial),
    family_mapping = family_mapping,
    data = mresp_hsced
  )
  expect_snapshot(print(summary(mod), digits = 2))
})
