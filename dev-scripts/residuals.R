devtools::load_all()
library(lme4)

gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             data = cbpp, family = binomial)
gm2 <- galamm(cbind(incidence, size - incidence) ~ period + (1 | herd),
              data = cbpp, family = binomial)


plot(residuals(gm1, type = "deviance"), residuals(gm2, type = "deviance"))
abline(0, 1)
