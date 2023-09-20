test_that("Heteroscedastic model works", {
  mod <- galamm(
    formula = y ~ x + (1 | id),
    weights = ~ (1 | item),
    data = hsced
  )

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
})
