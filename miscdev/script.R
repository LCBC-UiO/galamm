devtools::load_all()

formula = y ~ x + (1 | id)
data = dat1
family = binomial()
nAGQ = 5


mod0 <- lme4::glmer(
  formula = y ~ x + (1 | id),
  data = dat1,
  family = binomial(),
  nAGQ = 5
  )
lme4::getME(mod0, "theta")
lme4::getME(mod0, "fixef")
