devtools::load_all()
library(Matrix)
library(tidyverse)
library(lme4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(memoise)
library(mvtnorm)

dat <- tibble(id = 1:1000) %>%
  mutate(
    random_intercept = rnorm(nrow(.))
  ) %>%
  uncount(9, .id = "tp") %>%
  mutate(
    group = if_else(tp %in% 1:3, 1, if_else(tp %in% 4:6, 2, 3)),
    x = runif(nrow(.)),
    y = case_when(
      group %in% c(1, 2) ~ x + random_intercept + rnorm(nrow(.), sd = if_else(group == 1, .1, .5)),
      group == 3 ~ as.numeric(rbinom(nrow(.), 1, plogis(x + random_intercept))),
      TRUE ~ NA_real_
    )
  )

lmod <- lFormula(y ~ x + (1 | id), data = dat)
theta_inds <- seq_along(lmod$reTrms$theta)
beta_inds <- seq(from = max(theta_inds) + 1L, length.out = ncol(lmod$X))
weights_mapping <- if_else(dat$group == 2, 0L, -1L)
weights_inds <- max(beta_inds) + 1L

mlwrapper <- function(par, hessian = FALSE){
  marginal_likelihood(
    y = dat$y,
    trials = rep(1, length(dat$y)),
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    theta_mapping <- lmod$reTrms$Lind - 1L,
    beta = par[beta_inds],
    theta = par[theta_inds],
    weights = par[weights_inds],
    weights_mapping = weights_mapping,
    family = c("binomial", "gaussian"),
    family_mapping = if_else(dat$group %in% 1:2, 1L, 0L),
    maxit_conditional_modes = 1L,
    hessian = hessian
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par){
  mlmem(par)$logLik
}
gr <- function(par){
  mlmem(par)$gradient
}

par_init <- c(lmod$reTrms$theta, rep(0, length(beta_inds)), rep(1, length(weights_inds)))
lbound <- c(lmod$reTrms$lower, rep(-Inf, length(beta_inds)), rep(.01, length(weights_inds)))

opt <- optim(
  par = par_init, fn = fn, gr = gr,
  method = "L-BFGS-B", lower = lbound,
  control = list(fnscale = -1)
)
final_model <- mlwrapper(opt$par, hessian = TRUE)

sqrt(final_model$phi)
sqrt(final_model$phi / opt$par[weights_inds])
sqrt(opt$par[[1]]**2 * final_model$phi[[1]])
