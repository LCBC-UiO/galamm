library(tidyverse)
library(galamm)
library(lme4)
library(memoise)
library(Matrix)
library(mgcv)
library(gamm4)
library(glue)
dat <- read_delim(
  "~/LCBC/Users/OysteinS/latent-variable-gamm/data/cognitive_data.txt",
  delim = ";",
  col_types = paste0("ff", paste(rep("d", 16), collapse = ""), collapse = "")
) %>%
  rename(id = subject_id, sex = subject_sex, age = visit_age) %>%
  pivot_longer(cols = -c(id, sex, age), names_to = "test", values_to = "value",
               values_drop_na = TRUE) %>%
  filter(test %in% c("wasi_vocab_raw", "digitspan_bwd")) %>%
  filter(age > 7, age < 15) %>%
  group_by(test) %>%
  mutate(
    value = if_else(test == "wasi_vocab_raw",
                    (value - mean(value)) / sd(value), value)
  ) %>%
  ungroup()

lmod <- lFormula(value ~ test + age + (1 | id), data = dat)

theta_inds <- seq_along(lmod$reTrms$theta)
beta_inds <- seq(from = max(theta_inds), length.out = ncol(lmod$X))
lbound <- c(lmod$reTrms$lower, rep(-Inf, length(beta_inds)))

mlwrapper <- function(par, hessian = FALSE, epsilon_u = 100) {
  marginal_likelihood(
    y = dat$value,
    trials = rep(16, length(dat$value)),
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
    family_mapping = if_else(dat$test == "wasi_vocab_raw", 0L, 1L),
    maxit_conditional_modes = 10L,
    hessian = hessian,
    epsilon_u = epsilon_u
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par, hessian = FALSE, epsilon_u = 100) mlmem(par, hessian, epsilon_u)$logLik
gr <- function(par, hessian = FALSE, epsilon_u = 100) mlmem(par, hessian, epsilon_u)$gradient

opt <- optim(c(1, 6, -5, 2), fn, gr, method = "L-BFGS-B",
             lower = c(0, -10, -10, -10), upper = c(30, 10, 10, 10),
             control = list(fnscale = -1))

fn(opt$par)
gr(opt$par)

opt <- optim(opt$par + runif(4), fn, gr, epsilon_u = 10, method = "L-BFGS-B",
             lower = c(0, -10, -10, -10), upper = c(30, 30, 10, 30),
             control = list(fnscale = -1))

fn(opt$par)
gr(opt$par)

mlmem(opt$par, hessian = TRUE)



