library(Matrix)
library(lme4)
library(nlme, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(memoise)

set.seed(11)
n <- 200
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

theta_inds <- 1
beta_inds <- 2:3
weights_inds <- 4
bounds <- c(0, -Inf, -Inf, 0)
weights_mapping <- if_else(dat$item == 1, -1L, 0L)
lmod <- lFormula(y ~ x + (1 | id), data = dat, REML = FALSE)

mlwrapper <- function(par, hessian = FALSE){
  marginal_likelihood(
    y = dat$y,
    trials = rep(1, length(dat$y)),
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = lmod$reTrms$Lind - 1L,
    weights = par[weights_inds],
    weights_mapping = weights_mapping,
    maxit_conditional_modes = 1,
    hessian = hessian
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par){
  mlmem(par)$logLik
}
gr <- function(par, weights, weights_mapping){
  mlmem(par)$gradient
}

par_init <- c(1, 0, 0, 1)

opt <- optim(par_init, fn = fn, gr = gr,
             method = "L-BFGS-B", lower = bounds,
             control = list(fnscale = -1))

mod <- lme(y ~ x, data = dat, random = list(id =~ 1),
           weights = varIdent(form =~ 1 | item), method = "ML")

fm <- mlwrapper(opt$par, TRUE)

expect_equal(fm$phi, sigma(mod)^2, tolerance = 1e-3)
expect_equal(opt$par, c(1.01465252011755, 0.128895226513452, 0.706228524663702, 0.501311933841373),
             tolerance = 1e-5)

expect_equal(as.numeric(opt$value), as.numeric(logLik(mod)), tolerance = 1e-3)
