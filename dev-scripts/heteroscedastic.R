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
    #winv = if_else(item == 1, 1, 2),
    y = x + b + rnorm(nrow(.), sd = 1)
  )

# mod <- lme(y ~ x, data = dat, random = list(id =~ 1),
#            weights = varIdent(form =~ 1 | item), method = "ML")

###
lmod <- lFormula(y ~ x + (1 | id), data = dat, REML = FALSE, weights = rep(2, nrow(dat)))
devfun <- do.call(mkLmerDevfun, lmod)
opt <- optimizeLmer(devfun)
mm <- mkMerMod(environment(devfun), opt, lmod$reTrms, fr = lmod$fr)
logLik(mm)

theta_inds <- 1
beta_inds <- 2:3
lambda_inds <- integer()
bounds <- c(0, -Inf, -Inf)

mlwrapper <- function(par, weights, hessian = FALSE){
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
    weights = weights,
    family = "gaussian",
    maxit_conditional_modes = 1,
    hessian = hessian
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par, weights){
  mlmem(par, weights)$logLik
}
gr <- function(par, weights){
  mlmem(par, weights)$gradient
}
par_init <- c(1, 0, 0)

for(w in c(.5, 1, 2)){
  opt <- optim(par_init, fn = fn, gr = gr,
               weights = rep(w, nrow(dat)),
               method = "L-BFGS-B", lower = bounds,
               control = list(fnscale = -1))

  print(opt$value)

}

fm <- mlwrapper(opt$par, TRUE)

fm$phi
sigma(mm)^2

logLik(mm)
opt$value
opt$par
c(getME(mm, "theta"), fixef(mm))
