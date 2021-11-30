devtools::load_all()

## random intercept
formula = y ~ x + (1 | id)
data = dat1
family = binomial()
nAGQ = 11
maxit_newton <- 10
maxit_moments = 10

## random coefficients
formula = y ~ x + (x | id)
data = dat2
family = binomial()
nAGQ = c(5, 5)
maxit_newton <- 10
maxit_moments = 10

mod <- galamm(
  formula = y ~ x + (x | id),
  data = dat2,
  family = binomial(),
  nAGQ = 11, maxit_moments = 4)



mod0 <- lme4::glmer(
  formula = y ~ x + (1 + x| id),
  data = dat2,
  family = binomial()
  )

summary(mod0)
