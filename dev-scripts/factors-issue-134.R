library(PLmixed)
library(galamm)
data("KYPSsim")

kyps.lam <- rbind(c( 1,  0),
                  c(NA,  0),
                  c(NA,  1),
                  c(NA, NA))

test <- galamm(esteem ~ 0 + hs + ms + (1 | sid), data = KYPSsim,
               factor = list(c("ms", "hs")), load.var = c("time"),
               lambda = list(kyps.lam))
