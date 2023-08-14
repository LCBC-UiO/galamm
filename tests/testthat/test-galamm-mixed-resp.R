test_that("Mixed response works", {
  mod <- galamm(
    formula = y ~ x + (0 + itemgroup | id),
    data = mresp,
    family = c(gaussian, binomial),
    family_mapping = ifelse(mresp$itemgroup == "a", 1L, 2L)
  )


  expect_equal(mod$loglik, -4619.20400538187)
  expect_equal(
    summary(mod)$AICtab,
    c(
      AIC = 3584.79272575342, BIC = 3622.55702359404, logLik = -4619.20400538187,
      deviance = 3572.79272575342, df.resid = 3994
    )
  )
  expect_equal(summary(mod)$Lambda, NULL)

  expect_equal(
    mod$pearson_residuals[c(4, 8, 11)],
    c(0.636362128441962, 1.03062400738279, -1.04468172201157)
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
  tmp <- summary(mod)
  expect_equal(tmp$Lambda, structure(c(1, 1, -0.133902605671257, NA, NA, 0.0512069460559361), dim = 3:2, dimnames = list(c("1", "2", "3"), c(
    "loading",
    "SE"
  ))))
})
