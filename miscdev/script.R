devtools::load_all()

## random intercept
formula = y ~ x + (1 | id)
data = dat1
family = binomial()
nAGQ = 11
maxit_newton <- 10
maxit_moments = 10

mod0 <- lme4::glmer(
  formula = formula,
  data = dat1,
  family = binomial(),
  nAGQ = 11
)


## random coefficients
formula = y ~ x + (x | id)
data = dat2
family = binomial()
nAGQ = c(5, 5)
maxit_newton <- 10
maxit_moments = 10


mod0 <- lme4::glmer(
  formula = formula,
  data = dat2,
  family = binomial()
  )

summary(mod0)

lme4::getME(mod0, "theta")
exp(pars[3:5])
