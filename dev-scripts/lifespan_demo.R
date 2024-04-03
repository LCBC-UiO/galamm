library(galamm)
lmat <- matrix(c(
  1, rep(NA, 6), rep(0, 6),
  rep(0, 7), 1, NA, rep(0, 4),
  rep(0, 9), 1, NA, NA, NA), ncol = 3)

mod_init <- galamm(
  formula = cbind(y, 16 - y) ~ 0 + retest:domain + test +
    sl(age, by = domain) + (0 + domain | id),
  data = lifespan,
  family = c(binomial, gaussian),
  family_mapping = ifelse(lifespan$domain == "execfun", 2, 1),
  control = galamm_control(
    optim_control = list(REPORT = 2, factr = 1e9, trace = 3, maxit = 2))
)

mod <- galamm(
  formula = cbind(y, 16 - y) ~ 0 + retest:domain + test +
    sl(age, by = domain,
       factor = c("epmem_ability", "wmem_ability", "execfun_ability")) +
     (0 + domainepmem:epmem_ability + domainwmem:wmem_ability +
        domainexecfun:execfun_ability | id),
  data = lifespan,
  family = c(binomial, gaussian),
  family_mapping = ifelse(lifespan$domain == "execfun", 2, 1),
  load.var = "test",
  lambda = lmat,
  factor = c("epmem_ability", "wmem_ability", "execfun_ability"),
  control = galamm_control(
    optim_control = list(REPORT = 2, factr = 1e9, trace = 3, maxit = 10))
)
beepr::beep()
summary(mod)
plot_smooth(mod, scale = 0, pages = 1)
