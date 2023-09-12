rm(list=ls())
devtools::load_all()

formula = y ~ 0 + item + s(x, by = loading, k = 4) + (0 + loading | id)
weights = NULL
data = subset(cognition, domain == 1)
data$item <- factor(data$item)
family = gaussian
family_mapping = rep(1L, nrow(data))
load.var = "item"
lambda = list(matrix(c(1, NA, NA), ncol = 1))
factor = list("loading")
start = NULL
control = galamm_control()


