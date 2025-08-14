#' @srrstats {G5.0} Datasets from PLmixed package are used for testing, and
#'   results from the functions in this package are precomputed for comparison,
#'   in cases where PLmixed and galamm support the same models. In addition,
#'   dataset "sleepstudy" from lme4 package is also used here.
#' @srrstats {G5.4} It has been confirmed that PLmixed returns the same results.
#'   PLmixed is not run inside the tests, because it is too slow for that.
#' @srrstats {G5.8} Edge condition tests implemented here.
#' @srrstats {G5.8a} Tested below.
#' @srrstats {G5.8b} Data of wrong type is being tested.
#' @srrstats {G5.8c} All-NA fields will cause an error, since there will be no
#'   data.
#' @srrstats {G5.8d} Data with more columns than rows will cause failure,
#'   because the design matrix will be rank deficient. This is tested here.
#' @srrstats {RE2.4a,RE2.4.b,RE7.0,RE7.0a,RE7.1,RE7.1a}
#' @srrstats {RE7.2,RE7.3,RE7.4}
#' @noRd
NULL

test_that("wrong input is handled properly", {
  dat <- subset(cognition, domain == 2)

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = matrix(c(1, NA, NA), ncol = 1),
      factor = "loading"
    ),
    "lambda matrix must contain one row for each element in load.var"
  )

  expect_error(
    mod <- galamm(
      formula = as.character(y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint)),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = matrix(c(1, NA, NA), ncol = 1),
      factor = "loading"
    ),
    "formula must be a formula"
  )

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = matrix(c(1, NA), ncol = 1),
      factor = "loading",
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
      lambda = matrix(c(1, NA), ncol = 1),
      factor = "loading",
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
      lambda = matrix(c(1, NA), ncol = 1),
      factor = "loading",
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
      lambda = matrix(c(1, NA), ncol = 1),
      factor = "loading",
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
      lambda = matrix(c(1, NA), ncol = 1),
      factor = "loading",
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

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
      data = newdat,
      family = binomial,
      load.var = "item",
      lambda = matrix(c(1, NA, NA), ncol = 1),
      factor = 1L
    ),
    "factor must be NULL or a character vector"
  )

  newdat$loading <- 1
  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) +
        (0 + loading | id / timepoint),
      data = newdat,
      family = binomial,
      load.var = "item",
      lambda = list(matrix(c(1, NA), ncol = 1)),
      factor = "loading"
    ),
    "lambda must either be NULL or a matrix or numeric vector"
  )

  expect_error(
    mod <- galamm(formula = y ~ (1 | id), data = subset(dat, FALSE)),
    "No data, nothing to do."
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

  expect_error(
    {
      mod <- galamm(
        formula = y ~ x + (1 | id), data = dat, family = gaussian,
        control = galamm_control(pirls_tol_abs = 0)
      )
    },
    "pirls_tol_abs should be a strictly positive number"
  )

  expect_error(
    {
      mod <- galamm(
        formula = y ~ x + (1 | id), data = dat, family = gaussian,
        control = galamm_control(pirls_tol_abs = -.01)
      )
    },
    "pirls_tol_abs should be a strictly positive number"
  )

  expect_error(
    {
      mod <- galamm(
        formula = y ~ x + (1 | id), data = dat, family = gaussian,
        control = galamm_control(maxit_conditional_modes = 0)
      )
    },
    "maxit_conditional_modes should be a single positive integer"
  )

  expect_error(
    {
      mod <- galamm(
        formula = y ~ x + (1 | id), data = dat, family = gaussian,
        control = galamm_control(maxit_conditional_modes = 1:3)
      )
    },
    "maxit_conditional_modes should be a single positive integer"
  )

  expect_error(
    {
      mod <- galamm(
        formula = y ~ x + (1 | id), data = dat, family = gaussian,
        control = galamm_control(reduced_hessian = "yes")
      )
    },
    "reduced_hessian should be a logical of length one"
  )

  expect_error(
    {
      mod <- galamm(
        formula = y ~ x + (1 | id), data = dat, family = gaussian,
        control = galamm_control(reduced_hessian = c(TRUE, FALSE))
      )
    },
    "reduced_hessian should be a logical of length one"
  )

  expect_error(
    {
      mod1 <- galamm(
        formula = y ~ x + (1 | id), data = dat, family = gaussian,
        load.var = 1
      )
    },
    "load.var must be NULL or a character of length one"
  )

  expect_error(
    {
      mod1 <- galamm(
        formula = y ~ x + (0 + a | id), data = dat, family = gaussian,
        load.var = letters
      )
    },
    "load.var must be NULL or a character of length one"
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

  expect_equal(
    extract_optim_parameters(mod1),
    list(
      theta = 0.882229439513062,
      beta = c(-0.031119368224246, 0.412581926809024),
      lambda = numeric(0), weights = numeric(0)
    )
  )

  expect_equal(logLik(mod1), logLik(mod2))
  expect_equal(logLik(mod2), logLik(mod3))

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
    formula = esteem ~ as.factor(time) + (0 + hs | hid)
      + (0 + ms | mid),
    data = KYPSsim,
    factor = c("ms", "hs"),
    load.var = c("time"),
    lambda = kyps.lam,
    control = galamm_control(
      optim_control = list(maxit = 1),
      maxit_conditional_modes = 1
    )
  )

  expect_s3_class(kyps.model, "galamm")
  expect_snapshot(print(summary(kyps.model), digits = 2))

  # Model with factor loading on fixed effect
  KYPSsim$time2 <- as.numeric(KYPSsim$time == 2)
  kyps.model <- galamm(
    formula = esteem ~ 1 + ms:time2 + (1 | sid),
    data = subset(KYPSsim, time %in% c(1, 2)),
    factor = "ms", load.var = "time",
    lambda = matrix(c(1, NA)),
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


data("IRTsim", package = "PLmixed")
IRTsub <- IRTsim[IRTsim$item < 4, ] # Select items 1-3
set.seed(12345)
IRTsub <- IRTsub[sample(nrow(IRTsub), 300), ] # Randomly sample 300 responses

IRTsub <- IRTsub[order(IRTsub$item), ] # Order by item
irt.lam <- matrix(c(1, NA, NA), ncol = 1) # Specify the lambda matrix


test_that("missing values are handled appropriately", {
  expect_error(
    galamm(
      formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
      data = IRTsub,
      load.var = c("item"),
      factor = c("abil.sid"),
      lambda = irt.lam,
      na.action = na.fail
    ),
    "'arg' must be NULL or a character vector"
  )


  IRTsub$y[1] <- NA_real_

  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
        data = IRTsub,
        load.var = c("item"),
        factor = c("abil.sid"),
        lambda = irt.lam,
        na.action = "na.fail"
      )
    },
    "missing values in object"
  )

  options(na.action = "na.fail")

  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
        data = IRTsub,
        load.var = c("item"),
        factor = c("abil.sid"),
        lambda = irt.lam
      )
    },
    "missing values in object"
  )

  options(na.action = "na.pass")

  expect_error(
    galamm(
      formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
      data = IRTsub,
      load.var = c("item"),
      factor = c("abil.sid"),
      lambda = irt.lam
    ),
    "'arg' should be one of"
  )


  # Explicit argument vs relying on default
  mod <- galamm(
    formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
    data = IRTsub,
    load.var = "item",
    factor = "abil.sid",
    lambda = irt.lam,
    na.action = "na.omit"
  )
  options("na.action" = "na.omit")
  mod2 <- galamm(
    formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
    data = IRTsub,
    load.var = "item",
    factor = "abil.sid",
    lambda = irt.lam
  )
  options("na.action" = "na.exclude")
  mod3 <- galamm(
    formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
    data = IRTsub,
    load.var = "item",
    factor = "abil.sid",
    lambda = irt.lam
  )

  expect_equal(mod$model$deviance, mod2$model$deviance)
  expect_equal(mod$model$deviance, mod3$model$deviance)

  irt.lamInf <- irt.lam
  irt.lamInf[[1]] <- Inf
  expect_error(
    galamm(
      formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
      data = IRTsub,
      load.var = "item",
      factor = "abil.sid",
      lambda = irt.lamInf
    ),
    "elements of lambda can be either 0, 1, or NA"
  )
  irt.lamInf[[1]] <- NaN
  expect_error(
    galamm(
      formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
      data = IRTsub,
      load.var = "item",
      factor = "abil.sid",
      lambda = irt.lamInf
    ),
    "elements of lambda can be either 0, 1, or NA"
  )

  irt.lamInf[[1]] <- 2
  expect_error(
    galamm(
      formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
      data = IRTsub,
      load.var = "item",
      factor = "abil.sid",
      lambda = irt.lamInf
    ),
    "all non-NA values in lambda must be either 0 or 1"
  )

  IRTsub$y[1] <- -Inf

  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
        data = IRTsub,
        load.var = "item",
        factor = "abil.sid",
        lambda = irt.lam,
      )
    },
    "Infinite values"
  )

  IRTsub$y[1] <- 1
  IRTsub$y[13] <- Inf

  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
        data = IRTsub,
        load.var = "item",
        factor = "abil.sid",
        lambda = irt.lam,
      )
    },
    "Infinite values"
  )

  IRTsub$y[13] <- NaN

  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
        data = IRTsub,
        load.var = "item",
        factor = "abil.sid",
        lambda = irt.lam,
      )
    },
    "NaN in"
  )
})




test_that("edge conditions tests for data", {
  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
        data = IRTsub[0, ],
        load.var = "item",
        factor = "abil.sid",
        lambda = irt.lam,
      )
    },
    "No data, nothing to do."
  )

  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + (1 | school / sid),
        data = IRTsub[c(1, 94), ]
      )
    },
    "number of levels of each grouping factor must be < number of observations"
  )

  dat <- IRTsub
  dat$item <- rep(dat$item[[1]], length(dat$item))

  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + (1 | school),
        data = dat
      )
    },
    "contrasts can be applied only to factors with 2 or more levels"
  )

  dat <- IRTsub
  dat$y <- complex(dat$y)

  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + (1 | school),
        data = dat
      )
    },
    "Wrong R type for mapped vector"
  )
})

test_that("galamm rejects perfectly noiseless input data", {
  data("IRTsim", package = "PLmixed")
  dat <- IRTsim
  dat$y <- as.integer(dat$item)

  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + (1 | school),
        data = dat
      )
    },
    "Deterministic relationship between response and fixed effects."
  )

  dat <- IRTsim
  dat$item2 <- as.factor(dat$item)
  expect_error(
    {
      mod <- galamm(
        formula = y ~ 0 + as.factor(item) + as.factor(item2) + (1 | school),
        data = dat
      )
    },
    "the fixed effects will be jointly unidentifiable"
  )
})

test_that("loading and factor dimensions have to be correct", {
  data("IRTsim", package = "PLmixed")

  irt.lam <- matrix(c(1, NA, NA, NA, NA), ncol = 1)

  expect_error(
    galamm(
      formula = y ~ 0 + as.factor(item) + (0 + abil.sid | sid) +
        (0 + abil.sid | school),
      data = IRTsim,
      load.var = "item",
      factor = "abil.sid",
      lambda = irt.lam[1:4, , drop = FALSE]
    ),
    "lambda matrix must contain one row for each element in load.var"
  )
  expect_error(
    galamm(
      formula = y ~ 0 + as.factor(item) + (0 + abil.sid | sid) +
        (0 + abil.sid | school),
      data = IRTsim,
      load.var = "item",
      lambda = irt.lam
    ),
    "load.var, lambda, and factor must either all have values or all be NULL."
  )

  expect_message(
    galamm(
      formula = y ~ 0 + as.factor(item) + (0 + abil.sid | sid) +
        (0 + abil.sid | school),
      data = IRTsim,
      load.var = "item",
      factor = "abil.sid",
      lambda = as.numeric(irt.lam),
      control = galamm_control(
        optim_control = list(maxit = 1),
        maxit_conditional_modes = 1
      )
    ),
    "lambda converted to matrix with one column"
  )

  data("KYPSsim", package = "PLmixed")

  kyps.lam <- rbind(
    c(1, 0),
    c(NA, 0),
    c(NA, 1),
    c(NA, NA)
  )

  expect_error(
    galamm(
      formula = esteem ~ as.factor(time) + (0 + hs | hid) +
        (0 + ms | mid) + (1 | sid),
      data = KYPSsim,
      factor = c("ms", "hs"),
      load.var = "time",
      lambda = kyps.lam[, 1, drop = FALSE]
    ),
    "lambda matrix must have one column for each element in factor"
  )
})

test_that("errors are passed on from gamm4/mgcv", {
  dat <- data.frame(
    x0 = rep(1:5, each = 2),
    y  = rnorm(10)
  )

  err_galamm <- rlang::catch_cnd(
    galamm(y ~ s(x0, k = 20), data = dat),
    classes = "error"
  )
  err_gamm4 <- rlang::catch_cnd(
    gamm4::gamm4(y ~ s(x0, k = 20), data = dat),
    classes = "error"
  )

  expect_identical(class(err_galamm), class(err_gamm4))
  expect_identical(conditionMessage(err_galamm), conditionMessage(err_gamm4))

  warn_galamm <- rlang::catch_cnd(
    galamm(y ~ s(x0, k = 2), data = dat),
    classes = "warning"
  )
  warn_gamm4 <- rlang::catch_cnd(
    gamm4::gamm4(y ~ s(x0, k = 2), data = dat),
    classes = "warning"
  )

  expect_identical(class(warn_galamm), class(warn_gamm4))
  expect_identical(conditionMessage(warn_galamm), conditionMessage(warn_gamm4))

})
