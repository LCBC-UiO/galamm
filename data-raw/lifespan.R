# Simulate lifespan data from Section 4.1 in Sørensen et al. (2023)
library(tidyverse)
library(mvtnorm)
set.seed(3)
trajectories <- readRDS("data-raw/trajectories.rds")
epmem_fun <- approxfun(
  trajectories$age,
  trajectories$epmem_trajectory)
wmem_fun <- approxfun(
  trajectories$age, trajectories$wmem_trajectory)
execfun_fun <- approxfun(
  trajectories$age, trajectories$execfun_trajectory)

n_ids <- 1000
Psi3 <- matrix(c(.08, .05, .05,
                 .05, .12, .06,
                 .05, .06, .24), ncol = 3)
Psi2 <- diag(c(.06, .8, .18))

domains <- c("epmem", "wmem", "execfun")
epmem_tests <- c("CVLTA1", "CVLTA2", "CVLTA3", "CVLTA4", "CVLTA5",
                 "CVLT5min", "CVLT30min")
wmem_tests <- c("DspanBwd", "DspanFwd")
execfun_tests <- c("Stroop1", "Stroop2", "Stroop3", "Stroop4")

item_bias <- c(
  CVLTA1 = -.3,
  CVLTA2 = 0.7,
  CVLTA3 = 1.5,
  CVLTA4 = 1.8,
  CVLTA5 = 2.2,
  CVLT5min = 1.6,
  CVLT30min = 1.8,
  DspanBwd = -0.4,
  DspanFwd = 0.3,
  Stroop1 = 32,
  Stroop2 = 23,
  Stroop3 = 60,
  Stroop4 = 65
)
loadings <- c(1, 1.8, 2.4, 2.8, 3, 3, 3.2, 1, 1, 7, 4, 20, 20)
names(loadings) <- names(item_bias)

lifespan <- tibble(
  id = seq_len(n_ids),
  age_at_baseline = runif(n_ids, min = min(trajectories$age),
                          max = max(trajectories$age) - 10),
  timepoints = sample(6, n_ids, replace = TRUE, prob = c(.2, .4, .2, .1, .1, 1))
) %>%
  mutate(domain = list(domains)) %>%
  unnest(cols = "domain") %>%
  mutate(domain = factor(domain, levels = domains)) %>%
  nest_by(id, .keep = TRUE) %>%
  pmap_dfr(function(id, data) {
    data$zeta3 <- as.numeric(rmvnorm(1, sigma = Psi3))
    data
  }) %>%
  mutate(
    timepoint = map(timepoints, ~ seq_len(.x))
  ) %>%
  select(-timepoints) %>%
  unnest(cols = timepoint) %>%
  group_by(id, domain) %>%
  mutate(
    time_since_baseline = cumsum(c(0, runif(n(), min = .5, max = 2.5)[-1])),
    age = age_at_baseline + time_since_baseline
  ) %>%
  ungroup() %>%
  filter(between(age, min(trajectories$age), max(trajectories$age))) %>%
  select(-age_at_baseline, -time_since_baseline) %>%
  nest_by(id, timepoint, .keep = TRUE) %>%
  pmap_dfr(function(id, timepoint, data) {
    data$zeta2 <- as.numeric(rmvnorm(1, sigma = Psi2))
    data
  }) %>%
  mutate(
    eta = case_when(
      domain == "epmem" ~ epmem_fun(age),
      domain == "wmem" ~ wmem_fun(age),
      domain == "execfun" ~ -execfun_fun(age),
      TRUE ~ NA_real_
    ) + zeta2 + zeta3,
    test = case_when(
      domain == "epmem" ~ list(epmem_tests),
      domain == "wmem" ~ list(wmem_tests),
      domain == "execfun" ~ list(execfun_tests),
      TRUE ~ list(NA)
    )
  ) %>%
  unnest(cols = test) %>%
  mutate(
    retest_effect = case_when(
      domain == "epmem" ~ .05 * timepoint > 1,
      domain == "wmem" ~ .05 * timepoint > 1,
      domain == "execfun" ~ -2
    ),
    item_bias = item_bias[test],
    lambda = loadings[test],
    nu = retest_effect + item_bias + eta * lambda
  ) %>%
  select(-zeta2, -zeta3, -eta, -retest_effect, -item_bias, -lambda) %>%
  rowwise() %>%
  mutate(
    y = case_when(
      domain %in% c("epmem", "wmem") ~ rbinom(1, 16, prob = plogis(nu)),
      domain == "execfun" ~ rnorm(1, mean = nu, sd = 8),
      TRUE ~ NA_real_
    ),
    test = factor(test, levels = c(epmem_tests, wmem_tests, execfun_tests))
  ) %>%
  group_by(test) %>%
  mutate(
    retest = as.integer(timepoint > 1),
    y = case_when(
      domain == "execfun" ~ (y - mean(y)) / sd(y),
      TRUE ~ y
    )
  ) %>%
  ungroup() %>%
  select(-nu, -timepoint) %>%
  as.data.frame()

mm <- model.matrix(~ 0 + domain, data = lifespan)
lifespan <- cbind(lifespan, mm)

usethis::use_data(lifespan, overwrite = TRUE)

