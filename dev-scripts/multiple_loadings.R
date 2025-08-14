rm(list=ls())
devtools::load_all()

formula = response ~ 0 + item + lat.var:time2 + lat.var:time3 +
  lat.var:time4 + (0 + hs:lat.var | hid) + (0 + ms:lat.var | mid) +
  (0 + lat.var:time | id)


data("KYPSitemsim", package = "PLmixed")

time.lam <- rbind(c( 1,  0),  # Specify time lambda matrix
                  c(NA,  0),
                  c(NA,  1),
                  c(NA, NA))

item.lam <- matrix(c(1, NA, NA, NA, NA, NA), ncol = 1) # Specify item lambda matrix

KYPSitemsim$time2 <- (KYPSitemsim$time == 2) * 1
KYPSitemsim$time3 <- (KYPSitemsim$time == 3) * 1
KYPSitemsim$time4 <- (KYPSitemsim$time == 4) * 1
KYPSitemsim$item <- as.factor(KYPSitemsim$item)
KYPSitemsim$time <- as.factor(KYPSitemsim$time)

weights <- NULL
data <- KYPSitemsim
family <- gaussian
family_mapping <- rep(1L, nrow(data))
lambda <- list(time.lam, item.lam)
factor = list(c("ms", "hs"), "lat.var")
load_var = c("time", "item")
factor_interactions <- NULL
start <- NULL
control <- galamm_control()
na.action <- "na.omit"

# kyps.item.model <- galamm(
#   formula = formula,
#   data = data,
#   lambda = lambda,
#   factor = factor,
#   load_var = load_var)
#
#
