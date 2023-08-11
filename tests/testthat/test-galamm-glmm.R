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
