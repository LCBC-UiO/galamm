library(galamm)
library(lme4)

gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             data = cbpp, family = binomial)
plot(gm1)

gm2 <- galamm(cbind(incidence, size - incidence) ~ period + (1 | herd),
              data = cbpp, family = binomial)
plot(gm2)

plot(predict(gm1), residuals(gm1, type = "pearson"))
plot(fitted(gm2), residuals(gm2, type = "pearson"))

plot(fitted(gm2), predict(gm1, type = "response"))

plot(residuals(gm1, type = "pearson"), residuals(gm2, type = "pearson"))
hist(residuals(gm1, type = "pearson"))
abline(0,1)

plot(residuals(gm1, type = "deviance"), residuals(gm2, type = "deviance"))
abline(0,1)

computed_residual <- (cbpp$incidence - fitted(gm1)) / sqrt(cbpp$size * fitted(gm1) * (1 -))

plot(
  residuals(gm1, type = "pearson"),

)
