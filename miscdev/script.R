devtools::load_all()

formula = y ~ x + (1 | id)
data = dat1
family = binomial()
nAGQ = 5
mod <- galamm(
  formula = y ~ x + (1 | id),
  data = dat1,
  family = binomial(),
  nAGQ = 5
  )
