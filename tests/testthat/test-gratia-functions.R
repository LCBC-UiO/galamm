test_that("gratia functions work", {
  dat <- subset(cognition, domain == 1 & item == "11" & id < 10)
  dat$y <- dat$y[, 1]
  mod <- galamm(y ~ s(x) + (1 | id), data = dat)

  expect_s3_class(draw(mod), "ggplot")
  expect_s3_class(draw(derivatives(mod)), "ggplot")
  expect_s3_class(appraise(mod), "ggplot")
  expect_s3_class(derivatives(mod), "tbl")
  expect_s3_class(derivatives(mod), "derivatives")

  mod <- galamm(y ~ x + (1 | id), data = dat)
  expect_error(draw(mod), "No GAM object")
  expect_error(derivatives(mod), "No GAM object")
  expect_error(appraise(mod), "No GAM object")
})
