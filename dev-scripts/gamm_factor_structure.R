rm(list=ls())
devtools::load_all()

dat <- subset(cognition, domain %in% 1:2 & id < 5 & timepoint %in% c(1, 4, 8))
dat$domain <- factor(dat$domain)
dat$item <- factor(paste0(dat$domain, dat$item))
dat$item12 <- as.integer(dat$item == "12")
dat$item13 <- as.integer(dat$item == "13")
dat$item22 <- as.integer(dat$item == "22")

formula = y ~
  s(x, by = loading1, k = 4) +
  s(x, by = loading2, k = 4)
weights <- NULL
data <- dat
family <- c(gaussian, gaussian)
family_mapping <- ifelse(dat$domain == 1, 1L, 2L)
lambda <- list(matrix(c(1, NA, NA, 0, 0,
                        0, 0, 0, 1, NA), ncol = 2))
load.var <- "item"
factor <- list(c("loading1", "loading2"))
start <- NULL
control <- galamm_control(optim_control = list(trace = 3, maxit = 3))
