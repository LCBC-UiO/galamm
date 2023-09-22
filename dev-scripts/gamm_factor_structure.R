rm(list=ls())
devtools::load_all()
dat <- subset(cognition, domain %in% c(1, 3) & item %in% c("11", "31"))

mod <- galamm(formula = y ~ domain + s(x, by = domain), data = dat)

mod <- gamm4::gamm4(y ~ domain + s(x, by = domain), data = dat)
plot(mod$gam, pages = 1, scale = 0)
