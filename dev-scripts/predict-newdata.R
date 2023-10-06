rm(list = ls())
devtools::load_all()

object <- galamm(
  formula = y ~ lbas * treat + lage + v4 + (1 | subj),
  data = epilep, family = poisson
)


newdata <- epilep
type <- "link"
