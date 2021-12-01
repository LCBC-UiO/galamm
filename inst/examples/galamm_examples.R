mod <- galamm(
  formula = y ~ x + (1 | id),
  data = dat1,
  family = binomial(),
  nAGQ = 4, maxit_newton = 2, maxit_moments = 3)
