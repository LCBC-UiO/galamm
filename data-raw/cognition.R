library(tidyverse)
library(mgcv)
library(mvtnorm)
set.seed(123)
tps <- 8
tests <- c(3, 2, 4)
family <- c("gaussian", "binomial", "binomial")
trials <- c(1, 1, 10)
n <- 200
lambda <- list(
  c(1, 1.4, .3),
  c(1, 2),
  c(1, 1, 1, 2)
)

# Residual standard deviation for Gaussian model
sdeps1 <- .1

f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) {
  0.2 * x^11 * (10 * (1 - x))^6 + 10 *
    (10 * x)^3 * (1 - x)^10
}

funs <- list(f0, f1, f2)

# Random intercept at the between-timepoint level
covmat3 <- matrix(c(
  1, .3, .4,
  .3, 1, .5,
  .4, .5, 1
), ncol = 3)

zeta3 <- rmvnorm(n, sigma = covmat3)
colnames(zeta3) <- 1:3

cognition <- crossing(
  id = seq_len(n)
) %>%
  bind_cols(zeta3) %>%
  pivot_longer(cols = -id, names_to = "domain", values_to = "zeta3") %>%
  rowwise() %>%
  mutate(x = map(8, ~ sort(runif(.x))), timepoint = list(1:8)) %>%
  unnest(cols = c(x, timepoint)) %>%
  mutate(
    zeta2 = rnorm(nrow(.), sd = .5)
  ) %>%
  nest_by(domain, .keep = TRUE) %>%
  pmap_dfr(function(domain, data) {
    data %>%
      mutate(linpred = funs[[!!as.integer(domain)]](x) + zeta2 + zeta3)
  }) %>%
  mutate(tests = tests[as.integer(domain)]) %>%
  uncount(tests, .id = "item") %>%
  mutate(
    loading = map2_dbl(domain, item, ~ lambda[[as.integer(.x)]][as.integer(.y)]),
    trials = map_dbl(domain, ~ trials[[as.integer(.x)]])
  ) %>%
  mutate(
    y = case_when(
      domain == 1 ~ rnorm(nrow(.), mean = loading * linpred, sd = sdeps1),
      domain == 2 ~ as.numeric(
        rbinom(nrow(.), trials, prob = plogis(loading * linpred))
      ),
      domain == 3 ~ as.numeric(
        rbinom(nrow(.), trials, prob = plogis(loading * linpred))
      ),
      TRUE ~ NA_real_
    )
  ) %>%
  select(-zeta3, -zeta2, -linpred, -loading) %>%
  mutate(
    domain = factor(as.integer(domain)),
    item = factor(as.integer(item)),
    timepoint = factor(timepoint)
  ) %>%
  as.data.frame()

usethis::use_data(cognition, overwrite = TRUE)
