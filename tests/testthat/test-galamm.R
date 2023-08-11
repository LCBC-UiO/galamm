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
               c(0.875093100678968, 0.0443173993886863, 0.0209366325072532,
                 1.50157384326851))
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

test_that("Complex LMM works", {
  data("JUDGEsim")
  JUDGEsim$item <- factor(JUDGEsim$item)
  judge.lam <- rbind(c( 1,  0,  1,  0,  0,  0),
                     c(NA,  0, NA,  0,  0,  0),
                     c(NA,  0, NA,  0,  0,  0),
                     c( 0,  1,  0,  1,  0,  0),
                     c( 0, NA,  0, NA,  0,  0),
                     c( 0, NA,  0, NA,  0,  0),
                     c( 0,  0,  0,  0,  1,  0),
                     c( 0,  0,  0,  0, NA,  0),
                     c( 0,  0,  0,  0, NA,  0),
                     c( 0,  0,  0,  0,  0,  1),
                     c( 0,  0,  0,  0,  0, NA),
                     c( 0,  0,  0,  0,  0, NA))

  judge_galamm <- galamm(
    formula = response ~ 0 + item + (1 | class) +
      (0 + trait1.t + trait2.t + trait1.s + trait2.s | stu) +
      (0 + teacher1 + teacher2 | tch),
    data = JUDGEsim,
    lambda = list(judge.lam),
    load.var = "item",
    factor = list(c("teacher1", "teacher2", "trait1.t",
                    "trait2.t", "trait1.s", "trait2.s"))
  )

  expect_equal(judge_galamm$loglik, -56553.2785661794)
  expect_equal(judge_galamm$par,
               c(0.784430334881896, 0.566133764367859, 0.398568132799786, 0.200370294453492,
                 0.334085183988745, 0.00647882691731993, 0.235677264194959, 0.773891841821503,
                 0.337997663719164, 0.869131637864806, 0.498543199885178, 0.239475682251201,
                 0.482285028560489, 0, 3.39914518474943, 3.36263585640721, 3.36210081563546,
                 2.81984223040228, 2.93869388713431, 2.8771346729679, 3.4260794096106,
                 3.55223187960471, 3.59726951301179, 2.33415461561759, 2.90902811819496,
                 2.47043867252208, 1.12783179637647, 0.998580402300312, 0.972482963140881,
                 1.2190594859786, 1.09151685852923, 1.06570210817052, 1.05362302739479,
                 0.958118368867656, 1.32218004851917, 1.14483478564702, 0.873958594688374,
                 1.09623955388905))

})
