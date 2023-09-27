rm(list=ls())
devtools::load_all()
data("KYPSitemsim", package = "PLmixed")

time.lam <- rbind(c( 1,  0),  # Specify time lambda matrix
                  c(NA,  0),
                  c(NA,  1),
                  c(NA, NA))

item.lam <- c(1, NA, NA, NA, NA, NA) # Specify item lambda matrix

KYPSitemsim$time2 <- (KYPSitemsim$time == 2) * 1
KYPSitemsim$time3 <- (KYPSitemsim$time == 3) * 1
KYPSitemsim$time4 <- (KYPSitemsim$time == 4) * 1

formula = response ~ 0 + as.factor(item) + lat.var:time2 + lat.var:time3 +
  lat.var:time4 + (0 + hs:lat.var | hid) + (0 + ms:lat.var | mid) +
  (0 + lat.var:as.factor(time) | id)
weights <- NULL
data <- KYPSitemsim
family <- gaussian
family_mapping <- rep(1L, nrow(data))
lambda <- list(time.lam, item.lam)
factor = list(c("ms", "hs"), "lat.var")
load.var = c("time", "item")
factor_interactions <- NULL
start <- NULL
control <- galamm_control()

# kyps.item.model <- galamm(
#   formula = formula,
#   data = data,
#   lambda = lambda,
#   factor = factor,
#   load.var = load.var)
#
#
