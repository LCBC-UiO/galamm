test_that("galamm reproduces gamm4", {
  dat <- subset(cognition, domain == 1 & item == "11" & id < 50)

  mod <- galamm(formula = y ~ s(x), data = dat)
  mod_comp <- gamm4::gamm4(formula = y ~ s(x), data = dat, REML = FALSE)

  expect_equal(unname(mod$gam$edf), unname(mod_comp$gam$edf), tolerance = .001)
  expect_equal(mod$gam$Ve, mod_comp$gam$Ve, tolerance = .001)
  expect_equal(mod$gam$Vp, mod_comp$gam$Vp, tolerance = .001)
  expect_snapshot(print(summary(mod$gam), digits = 2))
  expect_type(plot_smooth(mod), "list")

  expect_equal(predict(mod), predict(mod_comp$gam), tolerance = .01)

  mod <- galamm(formula = y ~ t2(x), data = dat)
  mod_comp <- gamm4::gamm4(formula = y ~ t2(x), data = dat, REML = FALSE)
  expect_equal(unname(mod$gam$edf), unname(mod_comp$gam$edf), tolerance = .001)
  expect_equal(mod$gam$Ve, mod_comp$gam$Ve, tolerance = .001)
  expect_equal(mod$gam$Vp, mod_comp$gam$Vp, tolerance = .001)
  expect_snapshot(print(summary(mod$gam), digits = 2))
  expect_equal(predict(mod), predict(mod_comp$gam), tolerance = .01)

  mod <- galamm(formula = y ~ s(x, fx = TRUE) + (1 | id), data = dat)
  mod_comp <- gamm4::gamm4(
    formula = y ~ s(x, fx = TRUE),
    random = ~ (1 | id), data = dat, REML = FALSE
  )

  expect_equal(deviance(mod), deviance(mod_comp$mer), tolerance = .0001)
  expect_snapshot(print(summary(mod$gam), digits = 2))
  expect_equal(predict(mod), predict(mod_comp$gam), tolerance = .01)

  set.seed(1)
  dat <- mgcv::gamSim(verbose = FALSE, scale = .1)
  mod0 <- mgcv::gamm(y ~ s(x1) + t2(x2), data = dat, method = "ML")
  mod1 <- galamm(y ~ s(x1) + t2(x2), data = dat)
  expect_equal(as.numeric(deviance(mod0$lme)), deviance(mod1),
    tolerance = .0001
  )
  expect_equal(predict(mod0$gam), predict(mod1), tolerance = .01)

  set.seed(1)
  dat <- mgcv::gamSim(4, verbose = FALSE)
  mod0 <- gamm4::gamm4(y ~ fac + s(x2, by = fac), data = dat, REML = FALSE)
  mod1 <- galamm(formula = y ~ fac + s(x2, by = fac), data = dat)
  expect_equal(deviance(mod0$mer), deviance(mod1), tolerance = .0001)
  expect_snapshot(print(summary(mod1$gam), digits = 2))
  expect_equal(mod1$gam$edf, mod0$gam$edf, tolerance = .0001)
  expect_equal(predict(mod0$gam), predict(mod1), tolerance = .01)
  nd <- data.frame(fac = head(dat$fac, 6), x2 = runif(6))
  expect_equal(predict(mod0$gam, newdata = nd),
    predict(mod1, newdata = nd),
    tolerance = .01
  )

  mod0 <- gamm4::gamm4(y ~ s(x0, by = x2), data = dat, REML = FALSE)
  mod1 <- galamm(y ~ s(x0, by = x2), data = dat)
  expect_equal(mod0$gam$coefficients, mod1$gam$coefficients, tolerance = .0001)
  expect_equal(deviance(mod0$mer), deviance(mod1), tolerance = .0001)
  expect_snapshot(print(summary(mod1$gam), digits = 2))
  nd <- data.frame(x0 = runif(6), x2 = runif(6))
  expect_equal(predict(mod0$gam, newdata = nd),
    predict(mod1, newdata = nd),
    tolerance = .01
  )

  mod0 <- gamm4::gamm4(y ~ t2(x0, by = x2), data = dat, REML = FALSE)
  mod1 <- galamm(y ~ t2(x0, by = x2), data = dat)
  expect_equal(mod0$gam$coefficients, mod1$gam$coefficients, tolerance = .0001)
  expect_equal(deviance(mod0$mer), deviance(mod1), tolerance = .0001)
  expect_snapshot(print(summary(mod1$gam), digits = 2))
  expect_equal(predict(mod0$gam, newdata = nd),
    predict(mod1, newdata = nd),
    tolerance = .01
  )

  set.seed(1)
  dat <- mgcv::gamSim(1, verbose = FALSE, dist = "binary")
  mod0 <- gamm4::gamm4(y ~ s(x0), family = binomial, data = dat)
  mod1 <- galamm(y ~ s(x0), family = binomial, data = dat)
  expect_equal(deviance(mod0$mer), deviance(mod1), tolerance = .001)
  expect_equal(predict(mod0$gam, newdata = nd),
    predict(mod1, newdata = nd),
    tolerance = .01
  )
  expect_equal(predict(mod0$gam, newdata = nd, type = "response"),
    predict(mod1, newdata = nd, type = "response"),
    tolerance = .01
  )
  expect_equal(predict(mod0$gam, type = "response"),
    predict(mod1, type = "response"),
    tolerance = .01
  )

  mod0 <- gamm4::gamm4(y ~ s(x0, by = x2), data = dat, family = binomial)
  mod1 <- galamm(y ~ s(x0, by = x2), data = dat, family = binomial)
  expect_equal(mod0$gam$coefficients, mod1$gam$coefficients, tolerance = .1)
  expect_equal(deviance(mod0$mer), deviance(mod1), tolerance = .001)
  expect_snapshot(print(summary(mod1$gam), digits = 2))
  expect_equal(predict(mod0$gam, newdata = nd, type = "response"),
    predict(mod1, newdata = nd, type = "response"),
    tolerance = .01
  )
  expect_equal(predict(mod0$gam, type = "response"),
    predict(mod1, type = "response"),
    tolerance = .01
  )

  mod0 <- gamm4::gamm4(y ~ t2(x0, by = x2), data = dat, family = binomial)
  mod1 <- galamm(y ~ t2(x0, by = x2), data = dat, family = binomial)
  expect_equal(mod0$gam$coefficients, mod1$gam$coefficients, tolerance = .1)
  expect_equal(deviance(mod0$mer), deviance(mod1), tolerance = .001)
  expect_snapshot(print(summary(mod1$gam), digits = 2))
  expect_equal(predict(mod0$gam, type = "response"),
    predict(mod1, type = "response"),
    tolerance = .01
  )

  set.seed(1)
  dat <- mgcv::gamSim(1, scale = .1, verbose = FALSE, dist = "poisson")

  mod0 <- gamm4::gamm4(y ~ s(x2), data = dat, family = poisson)
  mod1 <- galamm(y ~ s(x2), data = dat, family = poisson)
  expect_equal(deviance(mod0$mer), deviance(mod1), tolerance = .001)
  expect_equal(mod0$gam$edf, mod1$gam$edf, tolerance = .1)
  expect_snapshot(print(summary(mod1$gam), digits = 2))
  expect_equal(predict(mod0$gam, newdata = nd),
    predict(mod1, newdata = nd),
    tolerance = .01
  )
  expect_equal(predict(mod0$gam, newdata = nd, type = "response"),
    predict(mod1, newdata = nd, type = "response"),
    tolerance = .01
  )
  expect_equal(predict(mod0$gam, type = "response"),
    predict(mod1, type = "response"),
    tolerance = .01
  )

  mod0 <- gamm4::gamm4(y ~ t2(x2, by = x0), data = dat, family = poisson)
  mod1 <- galamm(y ~ t2(x2, by = x0), data = dat, family = poisson)
  expect_equal(deviance(mod0$mer), deviance(mod1), tolerance = .001)
  expect_equal(mod0$gam$edf, mod1$gam$edf, tolerance = .1)
  expect_snapshot(print(summary(mod1$gam), digits = 2))
  expect_equal(predict(mod0$gam, newdata = nd),
    predict(mod1, newdata = nd),
    tolerance = .01
  )
  expect_equal(predict(mod0$gam, newdata = nd, type = "response"),
    predict(mod1, newdata = nd, type = "response"),
    tolerance = .01
  )
  expect_equal(predict(mod0$gam, type = "response"),
    predict(mod1, type = "response"),
    tolerance = .01
  )
})

test_that("Basic GAMM with factor structures works", {
  dat <- subset(cognition, domain == 1 & timepoint == 1)

  mod <- galamm(
    formula = y ~ 0 + item + sl(x, load.var = "loading"),
    data = dat,
    load.var = "item",
    lambda = list(matrix(c(1, NA, NA), ncol = 1)),
    factor = list("loading")
  )

  expect_equal(
    coef(mod)[1:3],
    c(
      item11 = 0.669888238636741, item12 = 0.938949881157583,
      item13 = 0.19327451279475
    ),
    tolerance = .0001
  )

  expect_equal(
    predict(mod$gam)[3:9],
    structure(c(
      `3` = -0.0468214579631071, `25` = 0.0227632205604528,
      `26` = 0.291824863081295, `27` = -0.453850505281539,
      `49` = -0.00253156792150957,
      `50` = 0.266530074599332, `51` = -0.479145293763501
    ), dim = 7L, dimnames = list(
      c("3", "25", "26", "27", "49", "50", "51")
    )),
    tolerance = .0001
  )

  expect_equal(
    vcov(mod),
    structure(c(
      0.00559985106383491, 6.76254095833776e-16, 8.7167665432172e-17,
      3.59298897606922e-10, 6.76254095833776e-16, 0.0055998510638353,
      1.15498696525652e-16, 4.76077386790633e-10, 8.7167665432172e-17,
      1.15498696525652e-16, 0.00559985106383441, 6.13653297234754e-11,
      3.59298897606922e-10, 4.76077386790633e-10, 6.13653297234754e-11,
      0.0639815815562496
    ), dim = c(4L, 4L)),
    tolerance = .0001
  )

  expect_equal(
    factor_loadings(mod),
    structure(c(
      1, 1.38890559188738, 0.285174466526739, NA, 0.206861514893889,
      0.125655999708447
    ), dim = 3:2, dimnames = list(c(
      "lambda1", "lambda2",
      "lambda3"
    ), c("loading", "SE")))
  )

  mod <- galamm(
    formula = y ~ 0 + item + t2l(x, load.var = "loading"),
    data = dat,
    load.var = "item",
    lambda = list(matrix(c(1, NA, NA), ncol = 1)),
    factor = list("loading")
  )

  expect_equal(
    coef(mod)[1:3],
    c(
      item11 = 0.585985043619259, item12 = 0.821880314450475,
      item13 = 0.169255594138125
    ),
    tolerance = .001
  )

  expect_equal(
    predict(mod$gam)[3:9],
    structure(
      c(
        `3` = 0.00564862054158604, `25` = 0.0151596217554541,
        `26` = 0.251054892586669, `27` = -0.401569827725681,
        `49` = -0.00924515260886517,
        `50` = 0.22665011822235, `51` = -0.42597460209
      ),
      dim = 7L, dimnames = list(
        c("3", "25", "26", "27", "49", "50", "51")
      )
    ),
    tolerance = .0001
  )

  expect_equal(
    vcov(mod),
    structure(c(
      0.0083132054251711, 0.00355423354003251, 0.000728096111493452,
      0.00610186781361705, 0.00355423354003251, 0.0107112518833507,
      0.00101403521195143, 0.00959339564679258, 0.000728096111493452,
      0.00101403521195143, 0.0059689273518403, 0.00196479761841795,
      0.00610186781361705, 0.00959339564679258, 0.00196479761841795,
      0.0246253221170968
    ), dim = c(4L, 4L)),
    tolerance = .0001
  )

  expect_equal(
    factor_loadings(mod),
    structure(
      c(
        1, 1.39530180832884, 0.286267927177228, NA, 0.212062073868377,
        0.128493620433266
      ),
      dim = 3:2,
      dimnames = list(
        c("lambda1", "lambda2", "lambda3"),
        c("loading", "SE")
      )
    )
  )
})

test_that("GAMM with factor structures and random effects works", {
  dat <- subset(cognition, domain == 1 & id < 50 & timepoint %in% c(1, 4, 8))

  mod <- galamm(
    formula = y ~ 0 + item + sl(x, load.var = "loading", k = 4) +
      (0 + loading | id / timepoint),
    data = dat,
    load.var = "item",
    lambda = list(matrix(c(1, NA, NA), ncol = 1)),
    factor = list("loading")
  )

  expect_equal(
    coef(mod),
    c(
      item11 = 1.15744460299196, item12 = 1.60878967518903,
      item13 = 0.341634130725673,
      `s(x):loadingFx1` = -0.146146004977857
    )
  )

  expect_equal(
    vcov(mod),
    structure(c(
      0.0139704055103376, 0.019404393983382, 0.00416969325689835,
      5.92964962109609e-11, 0.019404393983382, 0.0271611676668362,
      0.00582120760678741, 7.95171728770467e-11, 0.00416969325689835,
      0.00582120760678741, 0.00132204815534548, 1.71344935389941e-11,
      5.92964962109633e-11, 7.95171728772904e-11, 1.7134493539049e-11,
      0.0490470998802075
    ), dim = c(4L, 4L))
  )

  expect_equal(
    factor_loadings(mod),
    structure(c(
      1, 1.39607574182039, 0.29999430086666, NA, 0.0121378075498697,
      0.00737715422888654
    ), dim = 3:2, dimnames = list(c(
      "lambda1",
      "lambda2", "lambda3"
    ), c("loading", "SE")))
  )

  expect_equal(mod$gam$edf,
    c(
      item11 = 1.00000000000001, item12 = 1.00000000000003,
      item13 = 0.999999999999996,
      `s(x):loading.1` = 0.995315567672236,
      `s(x):loading.2` = 0.999757875374097,
      `s(x):loading.3` = 1.00000000000007
    ),
    tolerance = .1
  )

  expect_equal(mod$gam$coefficients,
    c(
      item11 = 1.15744460299196, item12 = 1.60878967518903,
      item13 = 0.341634130725673,
      `s(x):loading.1` = 0.601163762614973, `s(x):loading.2` = 3.02308664034339,
      `s(x):loading.3` = -0.146146004977857
    ),
    tolerance = .01
  )
})

test_that("galamm with by variables and loadings works", {
  dat <- subset(
    cognition,
    domain %in% c(1, 3) & item %in% c("11", "12", "31", "32")
  )
  dat <- cbind(
    dat,
    model.matrix(~ 0 + domain, data = dat)[, c("domain1", "domain3")]
  )
  lmat <- matrix(c(
    1, NA, 0, 0,
    0, 0, 1, NA
  ), ncol = 2)

  mod <- galamm(
    formula = y ~
      domain + sl(x, k = 4, by = domain, load.var = c("ability1", "ability3")) +
      (0 + domain1:ability1 + domain3:ability3 | id),
    data = dat,
    load.var = "item",
    lambda = list(lmat),
    factor = list(c("ability1", "ability3")),
    control = galamm_control(
      optim_control = list(maxit = 0), reduced_hessian = TRUE
    )
  )

  expect_equal(
    deviance(mod),
    23732.65,
    tolerance = .001
  )
})
