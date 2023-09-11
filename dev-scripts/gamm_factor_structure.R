devtools::load_all()

formula = y ~ 0 + loading + s(x, by = loading) + (0 + loading | id)
weights = NULL
data = subset(cognition, domain == 1)
family = gaussian
family_mapping = rep(1L, nrow(data))
load.var = "item"
lambda = list(matrix(c(1, NA, NA), ncol = 1))
factor = list("loading")
start = NULL
control = galamm_control()


mod <- galamm(formula = formula, data = data, load.var = load.var,
              lambda = lambda, factor = factor)

summary(mod)
