devtools::load_all()
library(PLmixed)
library(lme4)


irt.lam <- c(1, NA, NA, NA, NA)
plm <- PLmixed(
  y ~ 0 + as.factor(item) + (0 + abil.sid | sid) + (0 + abil.sid | school),
  data = IRTsim, load.var = "item",
  factor = list(c("abil.sid")), lambda = list(irt.lam), iter.count = FALSE)
summary(plm$lme4)

galamm_mod <- galamm(
  formula = y ~ 0 + as.factor(item) + (0 + abil.sid | sid) + (0 + abil.sid | school),
  data = IRTsim, load.var = "item",
  factor = list(c("abil.sid")), lambda = list(irt.lam))

object <- galamm_mod
sd(object$residuals)
sd(residuals(plm))
sqrt(galamm_mod$phi)



sigma(plm$lme4)
sqrt(galamm_mod$phi)

summary(galamm_mod)
