rm(list=ls())
devtools::load_all()

dat <- subset(cognition, domain %in% c(1, 3))
dat$domain <- factor(dat$domain)
dat$item <- factor(dat$item)
dat$loading1 <- NULL #c(1, 1.4, .3, 0, 0, 0, 0)[dat$item]
dat$loading3 <- NULL #c(0, 0, 0, 1, 1, 1, 2)[dat$item]

formula = y ~ domain +
  s(x, by = domain, load.var = c("loading1", "loading3"), k = 4)

weights <- NULL
data = dat
family <- gaussian
family_mapping <- rep(1L, nrow(data))
load.var <- "item"
lambda <- list(lambda <- matrix(c(
  1, 1, NA, 0, 0, 0, 0,
  0, 0, 0, 1, 1, 1, NA
), ncol = 2))
factor <- list(c("loading1", "loading3"))
start <- NULL
control = galamm_control()

