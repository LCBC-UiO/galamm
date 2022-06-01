sqrt((sum(residuals(fm1)^2) + sum(ranef(fm1)$Subject$`(Intercept)`^2 / getME(fm1, "theta")^2)) / 180)

(sum(residuals(fm1)^2) + sum(ranef(fm1)$Subject$`(Intercept)`^2 / getME(fm1, "theta")^2)) / 180

(sum(residuals(fm1)^2) + sum(obj$u^2 *  954.5278)) / 180

sum(residuals(fm1)^2) / (180 - sum(obj$u^2))


VarCorr(fm1)

plot(
  ranef(fm1)$Subject$`(Intercept)`^2 / getME(fm1, "theta")^2,
  obj$u^2 *  954.5278
); abline(0, 1)
