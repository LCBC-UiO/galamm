library(PLmixed)
test_that("LMM with simple factor works", {
  data("IRTsim") # Load the IRTsim data

  IRTsub <- IRTsim[IRTsim$item < 4, ] # Select items 1-3
  set.seed(12345)
  IRTsub <- IRTsub[sample(nrow(IRTsub), 300), ] # Randomly sample 300 responses

  IRTsub <- IRTsub[order(IRTsub$item), ] # Order by item
  irt.lam = matrix(c(1, NA, NA), ncol = 1) # Specify the lambda matrix

  mod <- galamm(
    y ~ 0 + as.factor(item) + (0 + abil.sid |sid) +(0 + abil.sid |school),
    data = IRTsub, load.var = c("item"),
    factor = list(c("abil.sid")), lambda = list(irt.lam)
  )

  expect_equal(mod$loglik, -193.563337783604)
})

test_that("LMM with two factors works", {
  data("KYPSsim")
  KYPSsim$time <- factor(KYPSsim$time)
  kyps.lam <- rbind(c( 1, 0),
                    c(NA, 0),
                    c(NA, 1),
                    c(NA, NA))

  kyps_model <- galamm(
    formula = esteem ~ time + (0 + hs | hid) + (0 + ms | mid) + (1 | sid),
    data = KYPSsim,
    factor = list(c("ms", "hs")),
    load.var = "time",
    lambda = list(kyps.lam)
  )

  expect_equal(kyps_model$loglik, -9681.98738847869)
  expect_equal(kyps_model$par[kyps_model$lambda_inds],
               c(0.875125492917495, 0.0443730786908383, 0.0209971796470513,
                 1.50159825655477))
})

test_that("LMM with two raters works", {
  data("JUDGEsim")
  JUDGEsim <- JUDGEsim[order(JUDGEsim$item), ] # Order by item
  JUDGEsim$item <- factor(JUDGEsim$item)

  judge.lam <- rbind(c( 1,  0),
                     c(NA,  0),
                     c(NA,  0),
                     c( 0,  1),
                     c( 0, NA),
                     c( 0, NA))

  judge_galamm <- galamm(
    formula = response ~ 0 + item + (1 | class) + (0 + teacher1 + teacher2 | tch),
    data = JUDGEsim[JUDGEsim$item %in% 1:6, ],
    lambda = list(judge.lam),
    load.var = "item",
    factor = list(c("teacher1", "teacher2")))

  expect_equal(judge_galamm$loglik, -55047.4993084257)
})
