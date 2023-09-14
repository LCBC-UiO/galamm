test_that("wrong input is handled properly", {
  dat <- subset(cognition, domain == 2)
  dat$item <- factor(dat$item)

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) + (0 + loading | id / timepoint),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = list(matrix(c(1, NA, NA), ncol = 1)),
      factor = list("loading")
    )
  )
})

test_that("family can be defined in three different ways", {
  dat <- hsced[1:100, ]
  mod1 <- galamm(
    formula = y ~ x + (1 | id), data = dat, family = gaussian,
    control = galamm_control(optim_control = list(maxit = 1))
  )
  mod2 <- galamm(
    formula = y ~ x + (1 | id), data = dat, family = "gaussian",
    control = galamm_control(optim_control = list(maxit = 1))
  )
  mod3 <- galamm(
    formula = y ~ x + (1 | id), data = dat, family = gaussian(),
    control = galamm_control(optim_control = list(maxit = 1))
  )

  expect_equal(logLik(mod1), logLik(mod2))
  expect_equal(logLik(mod2), logLik(mod3))
})