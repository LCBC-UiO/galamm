test_that("Mixed response works", {
  mod <- galamm(
    formula = y ~ x + (0 + loading | id),
    data = mresp,
    family = c(gaussian, binomial),
    family_mapping = ifelse(mresp$itemgroup == "a", 1L, 2L),
    load.var = "itemgroup",
    lambda = list(matrix(c(1, NA), ncol = 1)),
    factor = list("loading")
  )

  expect_equal(mod$loglik, -4619.34161314597)
  expect_equal(
    summary(mod)$AICtab,
    c(AIC = 9248.68322629193, BIC = 9280.15347449244, logLik = -4619.34161314597,
      deviance = 3633.06904319559, df.resid = 3995)
  )
  expect_equal(
    factor_loadings(mod),
    structure(c(1, 1.09504540466714, NA, 0.0998236819120686), dim = c(
      2L,
      2L
    ), dimnames = list(c("lambda1", "lambda2"), c("loading", "SE")))
  )

  expect_equal(
    mod$pearson_residuals[c(4, 8, 11)],
    c(0.665216300549201, 1.01118103950785, -1.1118436618218)
  )
})

test_that("Covariate measurement error model works", {
  lam <- matrix(c(1, 1, NA), ncol = 1)
  mod <- galamm(
    formula = y ~ item + (age * bus):chd
      + (age * bus):loading:fiber + (0 + loading | id),
    data = diet,
    family = c(gaussian, binomial),
    family_mapping = ifelse(diet$item == "chd", 2L, 1L),
    factor = list("loading"),
    load.var = "item",
    lambda = list(lam),
    start = list(theta = 10)
  )

  expect_equal(mod$loglik, -1372.16038649521)

  expect_equal(
    factor_loadings(mod),
    structure(c(1, 1, -0.133902605671257, NA, NA, 0.0512069460559361), dim = 3:2, dimnames = list(c("lambda1", "lambda2", "lambda3"), c("loading", "SE")))
  )
})
