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

  newdat <- dat
  newdat$loading <- 1
  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) + (0 + loading | id / timepoint),
      data = newdat,
      family = binomial,
      load.var = "item",
      lambda = list(matrix(c(1, NA, NA), ncol = 1)),
      factor = list("loading")
    ),
    "Factor already a column in data"
  )

  expect_error(
    mod <- galamm(formula = y ~ (1 | id), data = subset(dat, FALSE))
  )

  expect_error(galamm_control(optim_control = list(maximum_iterations = 10)),
               "Unknown control names")
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

test_that("multiple factors and factors in fixed effects are allowed", {

  data("KYPSsim", package = "PLmixed")

  kyps.lam <- rbind(
    c(1, 0),
    c(NA, 0),
    c(NA, 1),
    c(NA, NA)
  )

  test <- galamm(esteem ~ 0 + hs + ms:time + (1 | sid),
    data = KYPSsim,
    factor = list(c("ms", "hs")), load.var = c("time"),
    lambda = list(kyps.lam),
    control = galamm_control(optim_control = list(maxit = 1))
  )

  expect_s3_class(test, "galamm")
})

test_that("functions fail when they should", {
  data("sleepstudy", package = "lme4")
  sleepstudy_copy <- sleepstudy
  mod1 <- galamm(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  mod2 <- galamm(Reaction ~ Days + (1 | Subject), data = sleepstudy[-1, ])
  mod3 <- galamm(Reaction ~ Days + (1 | Subject), data = sleepstudy_copy)

  expect_error(anova(mod1, mod2), "not all fitted to the same size")
  expect_error(anova(mod1, mod3), "to the same data object")

  expect_error(confint(mod1, parm = "beta", level = 1.2))
  expect_error(confint(mod1, parm = "beta", level = c(.2, .3)))
  expect_error(confint(mod1), "is missing")


})
