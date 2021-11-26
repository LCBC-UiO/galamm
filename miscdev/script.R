devtools::load_all()

mod <- galamm(
  formula = y ~ x + (1 | id),
  data = dat1,
  family = binomial(),
  nAGQ = 11, maxit_moments = 4)



mod0 <- lme4::glmer(
  formula = y ~ x + (1 + x| id),
  data = dat2,
  family = binomial()
  )

summary(mod0)
