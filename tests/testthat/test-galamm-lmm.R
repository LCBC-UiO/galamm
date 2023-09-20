data("IRTsim", package = "PLmixed")
test_that("LMM with simple factor works", {
  IRTsub <- IRTsim[IRTsim$item < 4, ] # Select items 1-3
  set.seed(12345)
  IRTsub <- IRTsub[sample(nrow(IRTsub), 300), ] # Randomly sample 300 responses

  IRTsub <- IRTsub[order(IRTsub$item), ] # Order by item
  irt.lam <- matrix(c(1, NA, NA), ncol = 1) # Specify the lambda matrix

  mod <- galamm(
    formula = y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
    data = IRTsub,
    load.var = c("item"),
    factor = list(c("abil.sid")),
    lambda = list(irt.lam)
  )

  # Must test that it works also with tibbles
  class(IRTsub) <- c("tbl_df", "tbl", "data.frame")
  mod1 <- galamm(
    y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
    data = IRTsub, load.var = c("item"),
    factor = list(c("abil.sid")), lambda = list(irt.lam)
  )

  class(IRTsub) <- c("data.table", "data.frame")
  mod2 <- galamm(
    y ~ 0 + as.factor(item) + (0 + abil.sid | school / sid),
    data = IRTsub, load.var = c("item"),
    factor = list(c("abil.sid")), lambda = list(irt.lam)
  )

  expect_equal(mod$hessian, mod1$hessian)
  expect_equal(mod$hessian, mod2$hessian)

  expect_equal(
    sd(ranef(mod)$school$abil.sid),
    0.180652430814172
  )

  expect_output(
    lme4::.prt.call(mod$call),
    "Formula: y ~ 0 + as.factor(item) + (0 + abil.sid | school/sid)",
    fixed = TRUE
  )

  expect_equal(mod$model$loglik, -193.563337783604)
  expect_equal(
    summary(mod)$AICtab,
    c(
      AIC = 403.126675567209, BIC = 432.756935364458, logLik = -193.563337783604,
      deviance = 387.126675567209, df.resid = 292
    )
  )
  expect_equal(
    factor_loadings(mod),
    structure(c(
      1, 1.05448712819873, 1.02126746190855, NA, 0.217881890204074,
      0.236814881042725
    ), dim = 3:2, dimnames = list(
      c("lambda1", "lambda2", "lambda3"),
      c("abil.sid", "SE")
    ))
  )

  expect_equal(
    residuals(mod)[c(4, 8, 11)],
    c(0.0513522294535425, -0.181269847807669, 0.0759916652950277)
  )

  expect_snapshot(print(VarCorr(mod), digits = 2))

  expect_snapshot(round(confint(mod, parm = "beta"), 2))
  expect_snapshot(round(confint(mod, parm = "lambda"), 2))
  expect_snapshot(round(confint(mod, parm = "theta"), 2))
})

data("KYPSsim", package = "PLmixed")
test_that("LMM with two factors works", {
  # Making data small for it to run faster
  dat <- subset(KYPSsim, hid <= 5 & mid <= 5)
  dat$time <- factor(dat$time)
  kyps.lam <- rbind(
    c(1, 0),
    c(NA, 0),
    c(NA, 1),
    c(NA, NA)
  )

  form <- esteem ~ time + (0 + hs | hid) + (0 + ms | mid) + (1 | sid)
  kyps_model <- galamm(
    formula = form,
    data = dat,
    factor = list(c("ms", "hs")),
    load.var = "time",
    lambda = list(kyps.lam)
  )

  expect_equal(kyps_model$model$loglik, -33.7792632108483)
  expect_equal(kyps_model$parameters$parameter_estimates[kyps_model$parameters$lambda_inds],
    c(
      0.631763067142624, -0.114603879076418, 0.0145214343084242,
      1.07489949334559
    ),
    tolerance = 1e-4
  )

  expect_equal(
    llikAIC(kyps_model),
    c(
      AIC = 91.5585264216966, BIC = 115.202029384322, logLik = -33.7792632108483,
      deviance = 67.5585264216966, df.resid = 41
    )
  )

  expect_equal(
    residuals(kyps_model)[c(3, 22)],
    c(-0.411647523673306, 1.30437183262668),
    tolerance = 1e-4
  )

  expect_equal(
    residuals(kyps_model, type = "deviance")[c(3, 22)],
    c(-0.411647523673306, 1.30437183262668),
    tolerance = 1e-4
  )
})

data("JUDGEsim", package = "PLmixed")
test_that("LMM with two raters works", {
  dat <- subset(JUDGEsim, item %in% 1:6 & stu < 20)
  dat$item <- factor(dat$item)

  judge.lam <- rbind(
    c(1, 0),
    c(NA, 0),
    c(NA, 0),
    c(0, 1),
    c(0, NA),
    c(0, NA)
  )

  judge_galamm <- galamm(
    formula = response ~ 0 + item + (1 | class) + (0 + teacher1 + teacher2 | tch),
    data = dat,
    lambda = list(judge.lam),
    load.var = "item",
    factor = list(c("teacher1", "teacher2"))
  )

  expect_equal(deviance(judge_galamm), 2158.16155409666)
  expect_equal(
    logLik(judge_galamm),
    structure(-1079.08077704833, nobs = 840L, df = 15L, class = "logLik")
  )
  expect_equal(
    llikAIC(judge_galamm),
    c(
      AIC = 2188.16155409666, BIC = 2259.16258247423, logLik = -1079.08077704833,
      deviance = 2158.16155409666, df.resid = 825
    )
  )

  expect_equal(factor_loadings(judge_galamm),
    structure(c(
      1, 1.24788910616366, 0.947028051478771, 0, 0, 0,
      NA, 0.264534156451991, 0.228633827429379, NA, NA, NA, 0, 0, 0,
      1, 1.00426975887412, 1.10790222766263, NA, NA, NA, NA, 0.262801414362859,
      0.279745804150548
    ), dim = c(6L, 4L), dimnames = list(c(
      "lambda1",
      "lambda2", "lambda3", "lambda4", "lambda5", "lambda6"
    ), c(
      "teacher1",
      "SE", "teacher2", "SE"
    ))),
    tolerance = 1e-4
  )

  expect_equal(quantile(residuals(judge_galamm)),
    c(
      `0%` = -2.26827478113357, `25%` = -0.578443286096374, `50%` = -0.00403503141561323,
      `75%` = 0.55979417430233, `100%` = 2.22456795070249
    ),
    tolerance = 1e-4
  )
})

## Commented out because it takes 5-10 minutes to run.
#
# test_that("Complex LMM works", {
#   JUDGEsim$item <- factor(JUDGEsim$item)
#   judge.lam <- rbind(c( 1,  0,  1,  0,  0,  0),
#                      c(NA,  0, NA,  0,  0,  0),
#                      c(NA,  0, NA,  0,  0,  0),
#                      c( 0,  1,  0,  1,  0,  0),
#                      c( 0, NA,  0, NA,  0,  0),
#                      c( 0, NA,  0, NA,  0,  0),
#                      c( 0,  0,  0,  0,  1,  0),
#                      c( 0,  0,  0,  0, NA,  0),
#                      c( 0,  0,  0,  0, NA,  0),
#                      c( 0,  0,  0,  0,  0,  1),
#                      c( 0,  0,  0,  0,  0, NA),
#                      c( 0,  0,  0,  0,  0, NA))
#
#   judge_galamm <- galamm(
#     formula = response ~ 0 + item + (1 | class) +
#       (0 + trait1.t + trait2.t + trait1.s + trait2.s | stu) +
#       (0 + teacher1 + teacher2 | tch),
#     data = JUDGEsim,
#     lambda = list(judge.lam),
#     load.var = "item",
#     factor = list(c("teacher1", "teacher2", "trait1.t",
#                     "trait2.t", "trait1.s", "trait2.s"))
#   )
#
#   expect_equal(judge_galamm$model$loglik, -56553.2785661794)
#   expect_equal(judge_galamm$parameters$parameter_estimates,
#                c(0.784430334881896, 0.566133764367859, 0.398568132799786, 0.200370294453492,
#                  0.334085183988745, 0.00647882691731993, 0.235677264194959, 0.773891841821503,
#                  0.337997663719164, 0.869131637864806, 0.498543199885178, 0.239475682251201,
#                  0.482285028560489, 0, 3.39914518474943, 3.36263585640721, 3.36210081563546,
#                  2.81984223040228, 2.93869388713431, 2.8771346729679, 3.4260794096106,
#                  3.55223187960471, 3.59726951301179, 2.33415461561759, 2.90902811819496,
#                  2.47043867252208, 1.12783179637647, 0.998580402300312, 0.972482963140881,
#                  1.2190594859786, 1.09151685852923, 1.06570210817052, 1.05362302739479,
#                  0.958118368867656, 1.32218004851917, 1.14483478564702, 0.873958594688374,
#                  1.09623955388905), tolerance = 1e-4)
#
#   tmp <- summary(judge_galamm)
#   expect_equal(tmp$Lambda,
#                structure(c(1, 1.12783179425509, 0.998580401925909, 0, 0, 0,
#                            0, 0, 0, 0, 0, 0, NA, 0.035838597834991, 0.0334494645321601,
#                            NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 1, 0.972482962774902,
#                            1.21905948534512, 0, 0, 0, 0, 0, 0, NA, NA, NA, NA, 0.0303605948678942,
#                            0.0343626808329717, NA, NA, NA, NA, NA, NA, 1, 1.09151685865203,
#                            1.06570210905939, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0.0217214373201995,
#                            0.0214398508325773, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0,
#                            0, 1, 1.05362303061733, 0.958118371374549, 0, 0, 0, 0, 0, 0,
#                            NA, NA, NA, NA, 0.0257563813063262, 0.024602749572987, NA, NA,
#                            NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 1, 1.32218004297408, 1.1448347803669,
#                            0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0.0610912379818294, 0.0563316297031143,
#                            NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0.873958595211964,
#                            1.09623955372973, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.044174939624668,
#                            0.0487381242507442), dim = c(12L, 12L), dimnames = list(c("1",
#                                                                                      "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), c("teacher1",
#                                                                                                                                                   "SE", "teacher2", "SE", "trait1.t", "SE", "trait2.t", "SE", "trait1.s",
#                                                                                                                                                   "SE", "trait2.s", "SE"))),
#                tolerance = 1e-4
#   )
# })
