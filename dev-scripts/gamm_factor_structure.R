
rm(list=ls())
devtools::load_all()


formula = y ~ s(x, k = 4)
weights = NULL
data = subset(cognition, domain == 1 & item == 1)
data$item <- factor(data$item)
family = gaussian
family_mapping = rep(1L, nrow(data))
load.var = NULL
lambda = NULL
factor = NULL
start = NULL
control = galamm_control()


