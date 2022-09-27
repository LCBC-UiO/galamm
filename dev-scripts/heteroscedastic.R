library(nlme)
library(galamm)
library(tidyverse)
library(memoise)
library(lme4)

n <- 2000
dat <- tibble(
  id = 1:n,
  b = rnorm(n)
) %>%
  uncount(3, .id = "tp") %>%
  uncount(2, .id = "item") %>%
  mutate(
    x = runif(nrow(.)),
    winv = if_else(item == 1, 1, 2),
    y = x + b + rnorm(nrow(.), sd = winv)
  )


mod <- lme(y ~ x, data = dat, random = list(id =~ 1),
           weights = varIdent(form =~ 1 | item), method = "ML")



###
lmod <- lFormula(y ~ x + (1 | id), data = dat, REML = FALSE,
                 weights = 1 / winv)

theta_inds <- 1
beta_inds <- 2:3
lambda_inds <- integer()
alpha_inds <- 4
bounds <- c(0, -Inf, -Inf, 0)

mlwrapper <- function(par){
  marginal_likelihood(
    y = dat$y,
    trials = rep(1, length(dat$y)),
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = lmod$reTrms$Lind - 1L,
    lambda = par[lambda_inds],
    lambda_mapping_X = integer(),
    lambda_mapping_Zt = integer(),
    weights = 1 / dat$winv,
    family = "gaussian",
    maxit_conditional_modes = 1
  )
}


mlmem <- memoise(mlwrapper)
fn <- function(par){
  mlmem(par)$logLik
}
gr <- function(par){
  mlmem(par)$gradient
}
par_init <- c(1, 0, 0)
opt <- optim(par_init, fn = fn, gr = gr,
             method = "L-BFGS-B", lower = bounds,
             control = list(fnscale = -1))

par <- opt$par
