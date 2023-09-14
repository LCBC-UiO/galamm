test_that("galamm reproduces gamm4", {
  dat <- subset(cognition, domain == 1 & item == 1 & id < 50)

  mod <- galamm(formula = y ~ s(x), data = dat)
  mod_comp <- gamm4::gamm4(formula = y ~ s(x), data = dat, REML = FALSE)

  expect_equal(mod$gam$edf, mod_comp$gam$edf, tolerance = .001)
  expect_equal(mod$gam$Ve, mod_comp$gam$Ve, tolerance = .001)
  expect_equal(mod$gam$Vp, mod_comp$gam$Vp, tolerance = .001)

  expect_snapshot(print(summary(mod), digits = 2))
})

test_that("Basic GAMM with factor structures works", {
  dat <- subset(cognition, domain == 1 & timepoint == 1)
  dat$item <- factor(dat$item)

  mod <- galamm(
    formula = y ~ 0 + item + s(x, by = loading),
    data = dat,
    load.var = "item",
    lambda = list(matrix(c(1, NA, NA), ncol = 1)),
    factor = list("loading")
  )

  expect_equal(
    coef(mod),
    c(
      item1 = 0.575853007561604, item2 = 0.799928932139754, item3 = 0.178232652144315,
      `s(x):loadingFx1` = 0.568214146152226
    )
  )

  expect_equal(
    vcov(mod),
    structure(c(
      0.00574546768687868, 1.51191515296913e-15, 3.3681535853856e-16,
      5.03190335365757e-15, 1.51191515296913e-15, 0.0057454676868797,
      4.67952026543057e-16, 6.99075952818631e-15, 3.3681535853856e-16,
      4.67952026543057e-16, 0.0057454676868777, 1.55730895830692e-15,
      5.03190335365758e-15, 6.9907595281863e-15, 1.55730895830692e-15,
      0.00574569029879613
    ), dim = c(4L, 4L))
  )

  expect_equal(
    factor_loadings(mod),
    structure(c(
      1, 1.37647313319922, 0.288954660966359, NA, 0.226964851092118,
      0.138856542736255
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
    formula = y ~ 0 + item + s(x, by = loading, k = 4) + (0 + loading | id / timepoint),
    data = dat,
    load.var = "item",
    lambda = list(matrix(c(1, NA, NA), ncol = 1)),
    factor = list("loading")
  )

  expect_equal(
    coef(mod),
    c(
      item1 = 1.04044423214881, item2 = 1.48237126561939, item3 = 0.320061566107232,
      `s(x):loadingFx1` = 0.0912233007537318
    )
  )

  expect_equal(
    vcov(mod),
    structure(c(
      0.0159615498268288, 0.0221713476999988, 0.00482776133001998,
      -6.38349172124885e-10, 0.0221713476999988, 0.0309886559536267,
      0.00673350673004468, -8.873246825958e-10, 0.00482776133001998,
      0.00673350673004468, 0.00153143623644409, -1.93513879911897e-10,
      -6.38349172124901e-10, -8.87324682595824e-10, -1.93513879911901e-10,
      0.0885624989790584
    ), dim = c(4L, 4L))
  )

  expect_equal(
    factor_loadings(mod),
    structure(c(
      1, 1.39474722749327, 0.303703086576254, NA, 0.0112584313417628,
      0.00685387477826514
    ), dim = 3:2, dimnames = list(c(
      "lambda1",
      "lambda2", "lambda3"
    ), c("loading", "SE")))
  )

  expect_equal(mod$gam$edf,
    c(
      item1 = 0.999999999999999, item2 = 1.00000000000003, item3 = 1,
      `s(x):loading.1` = 0.995335564513248, `s(x):loading.2` = 0.999702742338268,
      `s(x):loading.3` = 1.00000000000003
    ),
    tolerance = .1
  )

  expect_equal(mod$gam$coefficients,
    c(
      item1 = 1.04044423214881, item2 = 1.48237126561939, item3 = 0.320061566107232,
      `s(x):loading.1` = -0.247640603209323, `s(x):loading.2` = 2.96415524738146,
      `s(x):loading.3` = 0.0912233007537315
    ),
    tolerance = .01
  )
})
