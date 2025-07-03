#' @srrstats {G5.0} Datasets from PLmixed package are used for testing, and
#'   results from the functions in this package are precomputed for comparison,
#'   in cases where PLmixed and galamm support the same models.
#' @srrstats {G5.4} It has been confirmed that PLmixed returns the same results.
#'   PLmixed is not run inside the tests, because it is too slow for that.
#' @noRd
NULL

data("IRTsim", package = "PLmixed")
test_that("Logistic GLMM with simple factor works", {
  dat <- subset(IRTsim, sid < 50)
  dat$item <- factor(dat$item)
  irt.lam <- matrix(c(1, NA, NA, NA, NA), ncol = 1)

  form <- y ~ item + (0 + abil.sid | school / sid)
  mod <- galamm(
    formula = form,
    data = dat,
    family = binomial,
    load.var = "item",
    factor = "abil.sid",
    lambda = irt.lam
  )

  expect_snapshot(summary(mod))

  expect_equal(
    logLik(mod),
    structure(-146.6622279195, nobs = 245L, df = 11L, class = "logLik")
  )
  expect_equal(
    llikAIC(mod),
    c(
      AIC = 315.324455839001, BIC = 353.838296154993, logLik = -146.6622279195,
      deviance = 224.697617162178, df.resid = 234
    )
  )
  expect_equal(
    factor_loadings(mod),
    structure(c(
      1, 1.07647038015976, 0.890409132940386, 0.735757163276656,
      1.204041457035, NA, 1.00116115172062, 0.59772403000322, 0.58869976284929,
      1.01429848408794
    ), dim = c(5L, 2L), dimnames = list(c(
      "lambda1",
      "lambda2", "lambda3", "lambda4", "lambda5"
    ), c("abil.sid", "SE"))),
    tolerance = 1e-4
  )

  expect_equal(
    residuals(mod, type = "deviance")[c(4, 8, 11)],
    c(-1.78690466704062, -0.703586683984688, -0.89579766346322)
  )

  expect_equal(
    residuals(mod, type = "pearson")[c(2, 3, 9)],
    c(0.34711634976693, 0.810706452322364, -0.987045133505393)
  )

  set.seed(1234)
  dat <- IRTsim
  dat$trials <- sample(1:10, nrow(dat), replace = TRUE)
  dat$y <- rbinom(
    n = nrow(dat), size = dat$trials,
    prob = predict(mod, type = "response")
  )
  dat <- subset(dat, sid < 50)

  galamm_mod_trials <- galamm(
    formula = cbind(y, trials - y) ~ item + (1 | school / sid),
    data = dat,
    family = binomial
  )
  ## Test results are confirmed in comparison to this model
  # tmp <- lme4::glmer(
  #   formula = cbind(y, trials - y) ~ item + (1 | school / sid),
  #   data = dat,
  #   family = binomial,
  # )
  expect_equal(
    logLik(galamm_mod_trials),
    structure(-389.886011265833, nobs = 245L, df = 4L, class = "logLik")
  )
  expect_equal(
    fixef(galamm_mod_trials),
    c(`(Intercept)` = 0.597623431596681, item = 0.0560576419151671),
    tolerance = 1e-4
  )

  expect_equal(
    llikAIC(galamm_mod_trials),
    c(
      AIC = 787.772022531666, BIC = 801.777055373845,
      logLik = -389.886011265833,
      deviance = 418.428625884762, df.resid = 241
    )
  )
})

test_that("Poisson GLMM works", {
  count_mod <- galamm(
    formula = y ~ lbas * treat + lage + v4 + (1 | subj),
    data = epilep,
    family = poisson
  )
  expect_equal(as.numeric(logLik(count_mod)), -665.358734786824)
  expect_equal(deviance(count_mod), 407.006167030425, tolerance = .0001)
  expect_equal(
    count_mod$parameters$parameter_estimates,
    c(
      0.501565551064086, 1.79356916017583, 0.884503952015789,
      -0.334962607667788,
      0.48458513737595, -0.161087431903879, 0.338389940944434
    )
  )
})

#' @srrstats {G5.9b} Algorithms are determinstic, so changing random seeds does
#'   not affect the results. This is tested here.
#' @srrstats {G5.9} Noise susceptibility tests here.
#' @noRd
NULL

test_that(
  "Algorithm is robust to trivial noise",
  {
    dat <- subset(IRTsim, sid < 50)
    dat$item <- factor(dat$item)
    irt.lam <- matrix(c(1, NA, NA, NA, NA), ncol = 1)

    form <- y ~ item + (0 + abil.sid | school / sid)
    set.seed(1)
    mod <- galamm(
      formula = form,
      data = dat,
      family = binomial,
      load.var = "item",
      factor = "abil.sid",
      lambda = irt.lam
    )
    set.seed(2)
    mod0 <- galamm(
      formula = form,
      data = dat,
      family = binomial,
      load.var = "item",
      factor = "abil.sid",
      lambda = irt.lam
    )

    expect_equal(deviance(mod), deviance(mod0))
    expect_equal(coef(mod), coef(mod0))
    expect_equal(vcov(mod), vcov(mod0))
    expect_equal(factor_loadings(mod), factor_loadings(mod0))
  }
)
