test_that("wrong input is handled properly", {
  dat <- subset(cognition, domain == 2)
  dat$item <- factor(dat$item)

  expect_error(
    mod <- galamm(
      formula = y ~ 0 + item + s(x, by = loading) + (0 + loading | id / timepoint),
      data = dat,
      family = binomial,
      load.var = "item",
      lambda = list(matrix(c(1, NA, NA), ncol = 1)),
      factor = list("loading")
    )
  )
})
