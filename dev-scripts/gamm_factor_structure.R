rm(list=ls())
devtools::load_all()
set.seed(1)
dat <- mgcv::gamSim(4, verbose = FALSE)

formula <- y ~ fac + s(x2, by = fac)
weights = NULL
data <- dat
family = gaussian
family_mapping = rep(1L, nrow(data))
load.var = NULL
lambda = NULL
factor = NULL
start = NULL
control = galamm_control()
