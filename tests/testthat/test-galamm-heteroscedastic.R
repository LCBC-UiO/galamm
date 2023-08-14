test_that("Heteroscedastic model works", {
  mod <- galamm(
    formula = y ~ x + (1 | id),
    weights = ~ (1 | item),
    data = hsced
  )

  expect_equal(mod$loglik, -2058.14021326104)
  expect_equal(
    summary(mod)$AICtab,
    c(
      AIC = 4126.28042652208, BIC = 4151.73081070096, logLik = -2058.14021326104,
      deviance = 4116.28042652208, df.resid = 1195
    )
  )
  expect_equal(summary(mod)$Lambda, NULL)

  expect_equal(
    mod$pearson_residuals[c(4, 8, 11)],
    c(0.879617156429014, -1.83921105781445, 0.769190482966503)
  )
})
