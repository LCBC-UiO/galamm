devtools::load_all()
library(Matrix)
library(lme4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(memoise)
library(mvtnorm)

n_groups <- 2
D <- diag(c(1, 3))
n_rep <- 10
set.seed(1)
trials <- 10
dat <- tibble(id = 1:300) %>%
  pmap_dfr(function(id){
    re <- matrix(rmvnorm(1, mean = rep(0, n_groups), sigma = D),
                 ncol = n_groups, dimnames = list(NULL, c("gaussian", "binomial")))

    tibble(id) %>%
      bind_cols(re)
  }) %>%
  pivot_longer(cols = -id, values_to = "b") %>%
  uncount(n_rep) %>%
  mutate(
    y = case_when(
      name == "gaussian" ~ rnorm(nrow(.), mean = b),
      name == "binomial" ~ as.numeric(rbinom(nrow(.), trials, plogis(b)))
    ),
    name = factor(name, levels = c("gaussian", "binomial")),
    name_gaussian = as.numeric(name == "gaussian"),
    name_binomial = as.numeric(name == "binomial")
  )

#lmod <- lFormula(y ~ (0 + name | id), data = dat)
lmod <- lFormula(y ~ (0 + name_gaussian | id) + (0 + name_binomial | id), data = dat)


mod1 <- lmer(y ~ (0 + name_gaussian | id), data = filter(dat, name == "gaussian"),
             REML = FALSE)
mod2 <- glmer(cbind(y, trials - y) ~ (0 + name_binomial | id), data = filter(dat, name == "binomial"),
              family = "binomial")

lmod$reTrms$theta
theta_inds <- unique(lmod$reTrms$Lind)
beta_inds <- seq(from = max(theta_inds) + 1, length.out = ncol(lmod$X))

mlwrapper <- function(par, epsilon_u, hessian = FALSE){
  stopifnot(intersect(beta_inds, theta_inds) == integer(0))
  stopifnot(length(par) == (length(beta_inds) + length(theta_inds)))

  marginal_likelihood(
    y = dat$y,
    trials = rep(trials, nrow(dat)),
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = lmod$reTrms$Lind - 1L,
    family = c("gaussian", "binomial"),
    family_mapping = if_else(dat$name == "gaussian", 0L, 1L),
    maxit_conditional_modes = 10,
    hessian = hessian,
    epsilon_u = epsilon_u
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par, epsilon_u){
  mlmem(par, epsilon_u)$logLik
}
gr <- function(par, epsilon_u){
  mlmem(par, epsilon_u)$gradient
}

par_init <- c(lmod$reTrms$theta, mean(fixef(mod1), fixef(mod2)))
fn(par_init, .001)
logLik(mod1) + logLik(mod2)

opt <- optim(c(1, 1, 1), fn, gr, epsilon_u = 1, method = "L-BFGS-B",
             lower = c(lmod$reTrms$lower, rep(-Inf, length(beta_inds))),
             control = list(fnscale = -1))
opt <- optim(opt$par, fn, gr, epsilon_u = .01, method = "L-BFGS-B",
             lower = c(lmod$reTrms$lower, rep(-Inf, length(beta_inds))),
             control = list(fnscale = -1))
opt <- optim(opt$par, fn, gr, epsilon_u = .001, method = "L-BFGS-B",
             lower = c(lmod$reTrms$lower, rep(-Inf, length(beta_inds))),
             control = list(fnscale = -1))

fmod <- mlmem(opt$par, 1e-4, TRUE)

Lambdat <- lmod$reTrms$Lambdat
Lambdat@x <- opt$par[lmod$reTrms$Lind]
fmod$phi[[1]] * unique((Lambdat %*% t(Lambdat))@x)

fmod$phi[[1]] * opt$par[[1]]^2
getME(mod1, "theta")^2

fmod$phi[[1]] * opt$par[[2]]^2
getME(mod2, "theta")^2

plot(fmod$u, c(getME(mod1, "u"), getME(mod2, "u"))); abline(0, 1)

opt$value
logLik(mod1) + logLik(mod2)

