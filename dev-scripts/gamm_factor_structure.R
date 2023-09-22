rm(list=ls())
devtools::load_all()
dat <- subset(cognition, domain %in% c(1, 3))
dat$domain1 <- as.numeric(dat$domain == 1)
dat$domain3 <- as.numeric(dat$domain == 3)
lmat <- matrix(c(1, NA, NA, 0, 0, 0, 0,
                 0, 0, 0, 1, NA, NA, NA), ncol = 2)

mod <- galamm(
  formula = y ~ 0 + domain +
    s(x, k = 4, by = domain, load.var = c("ability1", "ability3")),
  data = dat,
  load.var = "item",
  lambda = list(lmat),
  factor = list(c("ability1", "ability3"))
  )

