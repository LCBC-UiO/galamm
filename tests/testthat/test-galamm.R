test_that("galamm works", {
  load.mat <- matrix(c(1, NA), ncol = 1)
  dimnames(load.mat) <- list(c("item1", "item2"), NULL)
  ## Not run:
  mod <- galamm(
    formula = y ~ s(x, by = weight),
    random = ~(1|id),
    data = subset(dat1, item != "item3"),
    load.var = "item",
    lambda = load.mat,
    factor = "weight",
    optim_control = list(trace = 0)
  )
  expect_equal(c(mod$lambda_est), c(1, 2.00281079467529), tolerance = 1e-5)
})
