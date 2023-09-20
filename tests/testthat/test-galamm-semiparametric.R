test_that("galamm reproduces gamm4", {
  dat <- subset(cognition, domain == 1 & item == "11" & id < 50)

  mod <- galamm(formula = y ~ s(x), data = dat)
  mod_comp <- gamm4::gamm4(formula = y ~ s(x), data = dat, REML = FALSE)

  expect_equal(unname(mod$gam$edf), unname(mod_comp$gam$edf), tolerance = .001)
  expect_equal(mod$gam$Ve, mod_comp$gam$Ve, tolerance = .001)
  expect_equal(mod$gam$Vp, mod_comp$gam$Vp, tolerance = .001)

  mod <- galamm(formula = y ~ t2(x), data = dat)
  mod_comp <- gamm4::gamm4(formula = y ~ t2(x), data = dat, REML = FALSE)
  expect_equal(unname(mod$gam$edf), unname(mod_comp$gam$edf), tolerance = .001)
  expect_equal(mod$gam$Ve, mod_comp$gam$Ve, tolerance = .001)
  expect_equal(mod$gam$Vp, mod_comp$gam$Vp, tolerance = .001)

  mod <- galamm(formula = y ~ s(x, fx = TRUE) + (1 | id), data = dat)
  mod_comp <- gamm4::gamm4(
    formula = y ~ s(x, fx = TRUE),
    random = ~ (1 | id), data = dat, REML = FALSE
  )

  expect_equal(deviance(mod), deviance(mod_comp$mer), tolerance = .0001)
})

test_that("Basic GAMM with factor structures works", {
  dat <- subset(cognition, domain == 1 & timepoint == 1)
  dat$item <- factor(dat$item)

  mod <- galamm(
    formula = y ~ 0 + item + s(x, load.var = "loading"),
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
})

test_that("GAMM with factor structures and random effects works", {
  dat <- subset(cognition, domain == 1 & id < 50 & timepoint %in% c(1, 4, 8))
  dat$item <- factor(dat$item)

  mod <- galamm(
    formula = y ~ 0 + item + s(x, load.var = "loading", k = 4) +
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
