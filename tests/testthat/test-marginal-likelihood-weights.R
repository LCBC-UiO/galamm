library(Matrix)
library(lme4)
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

w <- 1 / dat$winv
lmod <- lFormula(y ~ x + (1 | id), data = dat, REML = FALSE, weights = w)
devfun <- do.call(mkLmerDevfun, lmod)
opt <- optimizeLmer(devfun)
mm <- mkMerMod(environment(devfun), opt, lmod$reTrms, fr = lmod$fr)

theta_inds <- 1
beta_inds <- 2:3
lambda_inds <- integer()
bounds <- c(0, -Inf, -Inf)

mlwrapper <- function(par, weights, weights_mapping, hessian = FALSE){
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
    weights_mapping = weights_mapping,
    family = "gaussian",
    maxit_conditional_modes = 1,
    hessian = hessian
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par, weights, weights_mapping){
  mlmem(par, weights, weights_mapping)$logLik
}
gr <- function(par, weights, weights_mapping){
  mlmem(par, weights, weights_mapping)$gradient
}

par_init <- c(1, 0, 0)
weights <- unique(w)
weights_mapping <- sapply(w, function(x) which(weights == x)) - 2L
opt <- optim(par_init, fn = fn, gr = gr,
             weights = weights[-1], weights_mapping = weights_mapping,
             method = "L-BFGS-B", lower = bounds,
             control = list(fnscale = -1))

fm <- mlwrapper(opt$par, weights[-1], weights_mapping, TRUE)

expect_equal(fm$phi, sigma(mm)^2, tolerance = 1e-3)
expect_equal(opt$par[[theta_inds]], getME(mm, "theta")[[1]], tolerance = 1e-3)
expect_equal(as.numeric(opt$value), as.numeric(logLik(mm)), tolerance = 1e-3)
expect_equal(opt$par[beta_inds], as.numeric(fixef(mm)), tolerance = 1e-3)

# Confirm that all equal weights are correct also
comp <- lmer(y ~ x + (1 | id), data = dat, REML = FALSE, weights = rep(2, nrow(dat)))
opt <- optim(par_init, fn = fn, gr = gr, weights = 2, weights_mapping = rep(0L, nrow(dat)),
             method = "L-BFGS-B", lower = bounds, control = list(fnscale = -1))

tmp <- mlwrapper(opt$par, weights = 2, weights_mapping = rep(0L, nrow(dat)), TRUE)
fn(c(getME(comp, "theta"), fixef(comp)), weights = 2, weights_mapping = rep(0L, nrow(dat)))
logLik(comp)

expect_equal(as.numeric(logLik(comp)), opt$value)
expect_equal(as.numeric(getME(comp, "theta")), opt$par[theta_inds], tolerance = 1e-4)
expect_equal(as.numeric(fixef(comp)), opt$par[beta_inds], tolerance = 1e-4)

