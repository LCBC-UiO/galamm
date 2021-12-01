test_that("galamm works", {
  mod <- galamm(
    formula = y ~ x + (1 | id),
    data = dat1,
    family = binomial(),
    nAGQ = 4, maxit_newton = 2, maxit_moments = 3)

  expect_equal(round(mod, 6),
               structure(c(0.206599, 0.126842, -0.638699), .Dim = c(3L, 1L)))
})
