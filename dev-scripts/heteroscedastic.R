library(nlme)
library(lme4)
library(galamm)
library(tidyverse)
library(memoise)
library(lme4)
set.seed(11)
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
    y = x + b + rnorm(nrow(.), sd = sqrt(winv))
  )

# mod <- lme(y ~ x, data = dat, random = list(id =~ 1),
#            weights = varIdent(form =~ 1 | item), method = "ML")

###
weights_mapping <- if_else(dat$item == 1, -1L, 0L)
w <- .5
lmod <- lFormula(y ~ x + (1 | id), data = dat, REML = FALSE,
                 weights = c(1, w)[weights_mapping + 2L])
devfun <- do.call(mkLmerDevfun, lmod)
opt <- optimizeLmer(devfun)
mm <- mkMerMod(environment(devfun), opt, lmod$reTrms, fr = lmod$fr)
cmp <- getME(mm, "devcomp")$cmp
dims <- getME(mm, "devcomp")$dims

theta_inds <- 1
beta_inds <- 2:3
weights_inds <- 4
bounds <- c(0, -Inf, -Inf, .1)

mlwrapper <- function(par, hessian = FALSE){
  marginal_likelihood(
    y = dat$y,
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = lmod$reTrms$Lind - 1L,
    weights = par[weights_inds],
    weights_mapping = weights_mapping,
    family = "gaussian",
    maxit_conditional_modes = 1,
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

par_init <- c(1, 0, 0, .5)
opt <- optim(par_init, fn = fn, gr = gr,
             method = "L-BFGS-B", lower = bounds,
             control = list(fnscale = -1))

fm <- mlwrapper(opt$par, TRUE)
fm$phi
fm$phi / opt$par[weights_inds]
