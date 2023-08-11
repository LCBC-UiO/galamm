devtools::load_all()
library(PLmixed)
data("IRTsim")
irt.lam <- c(1, NA, NA, NA, NA)
system.time({
  plm <- PLmixed(
    y ~ 0 + as.factor(item) + (0 + abil.sid | sid) + (0 + abil.sid | school),
    data = IRTsim, load.var = "item",
    factor = list(c("abil.sid")), lambda = list(irt.lam), iter.count = FALSE)
})
system.time({
  galamm_mod <- galamm(
    formula = y ~ 0 + as.factor(item) + (0 + abil.sid | sid) + (0 + abil.sid | school),
    data = IRTsim, load.var = "item",
    factor = list(c("abil.sid")), lambda = list(irt.lam))
})
galamm_mod
plm$`Log-Likelihood`

