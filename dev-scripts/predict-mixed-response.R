rm(list = ls())
devtools::load_all()

mod <- galamm(
  formula = y ~ x + (0 + loading | id),
  data = mresp,
  family = c(gaussian, binomial),
  family_mapping = ifelse(mresp$itemgroup == "a", 1L, 2L),
  load_var = "itemgroup",
  lambda = list(matrix(c(1, NA), ncol = 1)),
  factor = list("loading")
)

predict(mod)
