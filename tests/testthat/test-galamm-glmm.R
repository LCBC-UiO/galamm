library(PLmixed)
test_that("Logistic GLMM with simple factor works", {
  data("IRTsim")
  IRTsim$item <- factor(IRTsim$item)
  irt.lam <- matrix(c(1, NA, NA, NA, NA), ncol = 1)

  mod <- galamm(
    formula = y ~ item + (0 + abil.sid | sid) + (0 + abil.sid | school),
    data = IRTsim,
    family = binomial,
    load.var = "item",
    factor = list("abil.sid"),
    lambda = list(irt.lam)
  )

  expect_equal(mod$loglik, -1472.19990830883)
  expect_equal(summary(mod)$AICtab,
               c(AIC = 2394.27287296977, BIC = 2458.33737908919, logLik = -1472.19990830883,
                 deviance = 2372.27287296977, df.resid = 2489))
  expect_equal(summary(mod)$Lambda,
               structure(c(1, 0.737025403666384, 0.935110508605618, 0.606906510198586,
                           0.585991366477788, NA, 0.145578181193442, 0.187204916856415,
                           0.126094293895771, 0.116297718127772), dim = c(5L, 2L), dimnames = list(
                             c("1", "2", "3", "4", "5"), c("abil.sid", "SE"))))

  expect_equal(mod$deviance_residuals[c(4, 8, 11)],
               c(-1.72767556978008, -0.762888860949779, -0.822961819214384))

  expect_equal(mod$pearson_residuals[c(2, 3, 9)],
               c(0.571079113462046, 0.809777597885337, -1.13850440935134))
})

test_that("Poisson GLMM works", {
  count_mod <- galamm(
    formula = y ~ lbas * treat + lage + v4 + (1|subj),
    data = epilep,
    family = poisson
  )
  expect_equal(count_mod$loglik, -665.358734786824)
  expect_equal(count_mod$deviance, 407.006167030425)
  expect_equal(count_mod$par,
               c(0.501565551064086, 1.79356916017583, 0.884503952015789, -0.334962607667788,
                 0.48458513737595, -0.161087431903879, 0.338389940944434))

})

test_that("Logistic GLMM with multiple trials works", {
  set.seed(1234)
  dat <- IRTsim
  dat$trials <- sample(1:10, nrow(dat), replace = TRUE)
  dat$y <- rbinom(n = nrow(dat), size = dat$trials,
                  prob = predict(galamm_mod, type = "response"))

  galamm_mod_trials <- galamm(
    formula = y ~ item + (1 | sid) + (1 | school),
    data = dat,
    family = binomial,
    trials = dat$trials
  )
  ## Test results are confirmed in comparison to this model
  # tmp <- lme4::glmer(
  #   formula = cbind(y, trials - y) ~ item + (1 | sid) + (1 | school),
  #   data = dat,
  #   family = binomial,
  # )
  expect_equal(galamm_mod_trials$loglik, -3534.51945431292)
  expect_equal(summary(galamm_mod_trials)$fixef,
               structure(c(0.434857938909014, 0.355598397922802, -0.457362207203679,
                           0.513797577453551, 0.581753809430895, 0.165404285537448, 0.0630256521208169,
                           0.0618539379075891, 0.064439591822642, 0.0641691651190108, 2.6290608946197,
                           5.64212167517345, -7.39422941651648, 7.97332141500342, 9.06594013420543
               ), dim = c(5L, 3L), dimnames = list(c("(Intercept)", "item2",
                                                     "item3", "item4", "item5"), c("Estimate", "Std. Error", "t value"
                                                     ))))

  expect_equal(
    summary(galamm_mod_trials)$AICtab,
    c(AIC = 2642.69306896257, BIC = 2683.46139103857, logLik = -3534.51945431292,
      deviance = 2628.69306896257, df.resid = 2493)
  )
})
