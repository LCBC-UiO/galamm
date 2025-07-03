test_that("Heteroscedastic model works", {
  mod <- galamm(
    formula = y ~ x + (1 | id),
    weights = ~ (1 | item),
    data = hsced
  )

  expect_snapshot(print(summary(mod), digits = 3))
  expect_equal(mod$model$loglik, -2058.14021326104)
  expect_equal(
    summary(mod)$AICtab,
    c(
      AIC = 4126.28042652208, BIC = 4151.73081070096,
      logLik = -2058.14021326104,
      deviance = 4116.28042652208, df.resid = 1195
    )
  )
  expect_equal(summary(mod)$Lambda, NULL)

  expect_equal(
    residuals(mod)[c(4, 8, 11)],
    c(0.879617156429014, -1.83921105781445, 0.769190482966503)
  )

  expect_equal(vcov(mod, parm = "weights"),
    structure(0.00245961267414418, dim = c(1L, 1L)),
    tolerance = 1e-4
  )

  expect_equal(vcov(mod, parm = 1L),
    structure(0.00543386086733993, dim = c(1L, 1L)),
    tolerance = 1e-4
  )

  expect_error(vcov(mod, parm = 5L), "out of bounds")
  expect_error(vcov(mod, parm = "phi"), "Parameter not found")

  expect_error(
    galamm(
      formula = y ~ x + (1 | id),
      weights = list(~ (1 | item)),
      data = hsced
    ),
    "weights must be a formula"
  )


  # Now use initial values
  mod_start <- galamm(
    formula = y ~ x + (1 | id),
    weights = ~ (1 | item),
    data = hsced,
    start = list(beta = c(.13, .70), theta = 1.01, weights = .501)
  )

  expect_equal(
    mod_start$parameters$parameter_estimates,
    mod$parameters$parameter_estimates,
    tolerance = .01
  )
})

test_that("Heteroscedastic model works with more than one group", {
  set.seed(123)
  hsced2 <- hsced
  hsced2$item[hsced2$item == 2] <- sample(c(2, 3), sum(hsced2$item == 2), replace = TRUE)

  mod <- galamm(
    formula = y ~ x + (1 | id),
    weights = ~ (1 | item),
    data = hsced2
  )

  expect_snapshot(print(summary(mod), digits = 3))
  expect_equal(mod$model$loglik, -2058.13575138403, tolerance = 1e-4)
  expect_equal(
    summary(mod)$AICtab,
    c(AIC = 4128.27150276806, BIC = 4158.81196378272,
      logLik = -2058.13575138403,
      deviance = 4116.27150276806, df.resid = 1194)
  )
  expect_equal(summary(mod)$Lambda, NULL)

  expect_equal(
    residuals(mod)[c(4, 8, 11)],
    c(0.881516323889496, -1.83825643847652, 0.77018964982384)
  )

  expect_equal(vcov(mod, parm = "weights"),
               structure(c(0.00341412313076528, 0.00142678601570138, 0.00142678601570138,
                           0.00357562468779565), dim = c(2L, 2L)),
               tolerance = 1e-4
  )

  expect_equal(vcov(mod, parm = 1L),
               structure(0.00543350591426884, dim = c(1L, 1L)),
               tolerance = 1e-4
  )

  expect_error(vcov(mod, parm = 6L), "out of bounds")
  expect_error(vcov(mod, parm = "phi"), "Parameter not found")

  # Now use initial values
  mod_start <- galamm(
    formula = y ~ x + (1 | id),
    weights = ~ (1 | item),
    data = hsced2,
    start = list(beta = c(.13, .70), theta = 1.01, weights = c(.501, .44))
  )

  expect_equal(
    mod_start$parameters$parameter_estimates,
    mod$parameters$parameter_estimates,
    tolerance = .01
  )
})
