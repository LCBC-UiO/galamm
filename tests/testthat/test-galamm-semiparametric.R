test_that("galamm reproduces gamm4", {
  dat <- subset(cognition, domain == 1 & item == 1)

  mod <- galamm(formula = y ~ s(x), data = dat)
  mod_comp <- gamm4::gamm4(formula = y ~ s(x), data = dat, REML = FALSE)

  expect_equal(mod$gam$edf, mod_comp$gam$edf, tolerance = .001)
  expect_equal(mod$gam$Ve, mod_comp$gam$Ve, tolerance = .001)
  expect_equal(mod$gam$Vp, mod_comp$gam$Vp, tolerance = .001)

  expect_snapshot(print(summary(mod), digits = 2))
})

test_that("Basic GAMM with factor structures works", {
  dat <- subset(cognition, domain == 1)
  dat$item <- factor(dat$item)
  dat <- dat[dat$timepoint == 1, ]

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
  dat <- subset(cognition, domain == 1)
  dat$item <- factor(dat$item)

  mod <- galamm(
    formula = y ~ 0 + item + s(x, by = loading) + (0 + loading | id / timepoint),
    data = dat,
    load.var = "item",
    lambda = list(matrix(c(1, NA, NA), ncol = 1)),
    factor = list("loading")
  )

  expect_equal(
    coef(mod),
    c(
      item1 = 1.25074625523314, item2 = 1.75502419468873, item3 = 0.379089137221789,
      `s(x):loadingFx1` = 0.0203784039785214
    )
  )

  expect_equal(
    vcov(mod),
    structure(c(
      0.00455918825566397, 0.00637611723278559, 0.00136714728242274,
      -9.3818345565992e-11, 0.00637611723278559, 0.00893533275198847,
      0.00191456704177359, -1.31273713101223e-10, 0.00136714728242274,
      0.00191456704177359, 0.000416668471614241, -2.8153328293663e-11,
      -9.38183455660244e-11, -1.31273713101491e-10, -2.8153328293733e-11,
      0.0421377265832167
    ), dim = c(4L, 4L))
  )

  expect_equal(
    factor_loadings(mod),
    structure(c(
      1, 1.40041023113521, 0.300271618584508, NA, 0.00344146863189051,
      0.00208815232968891
    ), dim = 3:2, dimnames = list(c(
      "lambda1",
      "lambda2", "lambda3"
    ), c("loading", "SE")))
  )

  expect_snapshot(print(summary(mod), digits = 2))
})
