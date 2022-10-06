library(Matrix)
library(lme4)
library(memoise)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

set.seed(100)
dat <- tibble(id = 1:1000) %>%
  mutate(b = rnorm(nrow(.))) %>%
  uncount(4, .id = "item") %>%
  mutate(
    y = map2_dbl(
      b, item, ~ if_else(.y %in% c(1, 2), rnorm(1, .x), as.numeric(rbinom(1, 1, plogis(.x)))))
  )

lmod <- lFormula(y ~ (1 | id), data = dat)

theta_inds <- 1
beta_inds <- 2

mlwrapper <- function(par, hessian = FALSE, epsilon_u = .1){
  marginal_likelihood(
    y = dat$y,
    trials = rep(1, nrow(dat)),
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = lmod$reTrms$Lind - 1L,
    lambda = numeric(),
    lambda_mapping_X = integer(),
    lambda_mapping_Zt = integer(),
    weights = numeric(),
    weights_mapping = integer(),
    family = c("gaussian", "binomial"),
    family_mapping = if_else(dat$item %in% c(1, 2), 0L, 1L),
    maxit_conditional_modes = 10,
    hessian = hessian,
    epsilon_u = epsilon_u
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par, epsilon_u = .1){
  mlmem(par, epsilon_u = epsilon_u)$logLik
}
gr <- function(par, epsilon_u = .1){
  mlmem(par, epsilon_u = epsilon_u)$gradient
}

opt <- optim(c(1, 0), fn, gr, method = "L-BFGS-B",
             lower = c(0, -Inf), control = list(fnscale = -1))

opt <- optim(opt$par, fn, gr, epsilon_u = 1e-5, method = "L-BFGS-B",
             lower = c(0, -Inf), control = list(fnscale = -1))

fmod <- mlmem(opt$par, TRUE)

test_that("mixed response works", {
  expect_equal(opt$par, c(0.95732902946743, 0.0114817102301233))
  expect_equal(opt$value, -4646.14884571025)
  expect_equal(fmod$phi, 0.929084053682197)
  expect_equal(fmod$hessian, structure(c(-610.404791141158, -1.55077195603924, -1.55077195603924,
                                         -684.949541922932), dim = c(2L, 2L)))
})



