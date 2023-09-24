test_that("wrong input is handled properly", {
  dat <- subset(cognition, domain == 2)

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = list(matrix(c(1, NA, NA), ncol = 1)),
      factor = list("loading")
    )
  )

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = list(matrix(c(1, NA), ncol = 1)),
      factor = list("loading"),
      start = list(phi = 1)
    ),
    "Unknown names in initial value list"
  )

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = list(matrix(c(1, NA), ncol = 1)),
      factor = list("loading"),
      start = list(theta = 1)
    ),
    "Wrong number of elements"
  )

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = list(matrix(c(1, NA), ncol = 1)),
      factor = list("loading"),
      start = list(beta = rep(1, 10))
    ),
    "Wrong number of elements"
  )

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = list(matrix(c(1, NA), ncol = 1)),
      factor = list("loading"),
      start = list(lambda = rep(1, 5))
    ),
    "Wrong number of elements"
  )

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = list(matrix(c(1, NA), ncol = 1)),
      factor = list("loading"),
      start = list(weights = rep(1, 5))
    ),
    "Wrong number of elements"
  )

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + (1 | id),
      weights = ~ (1 | item) + (1 | domain),
      data = dat
    ),
    "Multiple grouping terms in weights not yet implemented."
  )


  newdat <- dat
  newdat$loading <- 1
  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
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

  expect_error(
    galamm_control(optim_control = list(maximum_iterations = 10)),
    "Unknown control names"
  )

  expect_error(galamm_control(method = "Quasi-Newton"))
  expect_error(
    galamm_control(
      method = "Nelder-Mead",
      optim_control = list(maxit = 2)
    ),
    "Unknown control names maxit"
  )
  expect_error(
    galamm_control(optim_control = list(xst = .001)),
    "Unknown control names xst"
  )

  expect_error(
    galamm_control(optim_control = list(fnscale = 2.3)),
    "fnscale parameter should be negative."
  )

  dat <- hsced[1:100, ]
  expect_error(mod1 <- galamm(
    formula = y ~ x + (1 | id), data = dat, family = gaussian,
    control = galamm_control(optim_control = list(fnscale = 2))
  ), "fnscale parameter should be negative.")

  expect_error(mod1 <- galamm(
    formula = y ~ x + (1 | id), data = dat, family = gaussian,
    control = galamm_control(optim_control = list(trace = -2))
  ), "trace should be a non-negative integer of length one")

  expect_error(mod1 <- galamm(
    formula = y ~ x + (1 | id), data = dat, family = gaussian,
    control = galamm_control(optim_control = list(trace = 1:4))
  ), "trace should be a non-negative integer of length one")
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

  expect_equal(
    extract_optim_parameters(mod1),
    list(theta = 0.882229439513062,
         beta = c(-0.031119368224246, 0.412581926809024),
         lambda = numeric(0), weights = numeric(0)))

  expect_equal(logLik(mod1), logLik(mod2))
  expect_equal(logLik(mod2), logLik(mod3))

  expect_error(
    predict(mod1, newdata = dat),
    "Not implemented yet"
  )

  expect_error(
    vcov(mod1, list(1:10)),
    "parm must be an integer or character vector"
  )

  expect_message(
    anova(mod1),
    "ANOVA tables for galamm objects not implemented yet."
  )
  expect_error(plot_smooth(mod1), "No terms to plot.")
})

test_that("multiple factors and factors in fixed effects are allowed", {
  data("KYPSsim", package = "PLmixed")
  kyps.lam <- rbind(
    c(1, 0), # Specify the lambda matrix
    c(NA, 0),
    c(NA, 1),
    c(NA, NA)
  )

  kyps.model <- galamm(
    esteem ~ as.factor(time) + (0 + hs | hid)
      + (0 + ms | mid),
    data = KYPSsim,
    factor = list(c("ms", "hs")), load.var = c("time"),
    lambda = list(kyps.lam),
    control = galamm_control(
      optim_control = list(maxit = 1),
      maxit_conditional_modes = 1
    )
  )

  expect_s3_class(kyps.model, "galamm")
  expect_snapshot(print(summary(kyps.model), digits = 2))

  # Model with factor loading on fixed effect
  KYPSsim$time2 <- as.numeric(KYPSsim$time == 2)
  kyps.model <- galamm(esteem ~ 1 + ms:time2 + (1 | sid),
    data = subset(KYPSsim, time %in% c(1, 2)),
    factor = list("ms"), load.var = c("time"),
    lambda = list(matrix(c(1, NA))),
    control = galamm_control(
      optim_control = list(maxit = 1),
      maxit_conditional_modes = 1
    )
  )

  expect_snapshot(print(summary(kyps.model), digits = 2))
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


test_that("multiple factors in fixed effects works", {
  path <-
    system.file("testdata", "test_multiple_factors.rds", package = "galamm")
  dat <- readRDS(path)

  lmat <- matrix(c(
    1, NA, NA, 0, 0, 0,
    0, 0, 0, 1, NA, NA
  ), ncol = 2)

  mod <- galamm(
    formula = y ~ 0 + x:domain1:lambda1 + x:domain2:lambda2 +
      (0 + 1 | id),
    data = dat,
    load.var = "item",
    lambda = list(lmat),
    factor = list(c("lambda1", "lambda2")),
    start = list(theta = 0.744468091602185,
                 beta = c(1.03995169865897, 1.87422267819485),
                 lambda = c(0.478791387562245, 1.94779433858618,
                            0.466484983394861, 2.02985361769537),
                 weights = numeric(0)),
    control = galamm_control(
      optim_control = list(FtolAbs = 1000,
                           FtolRel = 1000, XtolRel = 1000,
                           warnOnly = TRUE, xt = rep(1000, 7)),
      method = "Nelder-Mead")
  )
  expect_equal(deviance(mod), 7891.36597569292, tolerance = .001)
})
