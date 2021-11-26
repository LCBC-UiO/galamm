devtools::load_all()

mod <- galamm(
  formula = y ~ x + (1 | id),
  data = dat1,
  family = binomial(),
  nAGQ = 5, maxit_moments = 4)



mod0 <- lme4::glmer(
  formula = y ~ x + (1 | id),
  data = dat1,
  family = binomial(),
  nAGQ = 5
  )
lme4::getME(mod0, "theta")
lme4::getME(mod0, "fixef")
