rm(list=ls())
devtools::load_all()

dat <- subset(cognition, domain == 1)
dat$item <- factor(dat$item)
loading_matrix <- matrix(c(1, NA, NA), ncol = 1)

formula = y ~ s(x, k = 4, load.var = "loading") + (0 + loading | id)
weights <- NULL
data = dat
family <- gaussian
family_mapping <- rep(1L, nrow(data))
load.var <- "item"
lambda <- list(loading_matrix)
factor <- list("loading")
start <- NULL
control = galamm_control()

