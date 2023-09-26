library(tidyverse)

n <- 200

set.seed(1)
latent_covariates <- tibble(
  id = seq_len(n),
  type = list(c("measurement1", "measurement2", "response")),
  x = runif(n)
) %>%
  mutate(eta = rnorm(nrow(.))) %>%
  unnest(cols = type) %>%
  mutate(
    y = case_when(
      type == "measurement1" ~ eta,
      type == "measurement2" ~ eta * 1.3,
      type == "response" ~ .5 * x + eta * (-.3 + .2 * x)
    ) + rnorm(nrow(.), sd = .1),
    response = as.numeric(type == "response")
  ) %>%
  select(-eta) %>%
  as.data.frame()

usethis::use_data(latent_covariates, overwrite = TRUE)

set.seed(3)
n <- 1000
latent_covariates_long <- tibble(
  id = seq_len(n),
  type = list(c("measurement1", "measurement2", "response"))
) %>%
  mutate(eta = rnorm(nrow(.)), b = rnorm(nrow(.), sd = 1)) %>%
  unnest(cols = type) %>%
  mutate(repeated = if_else(type == "response", 5, 1)) %>%
  uncount(repeated, .id = "timepoint") %>%
  mutate(
    x = runif(nrow(.)),
    y = case_when(
      type == "measurement1" ~ eta,
      type == "measurement2" ~ eta * 1.3,
      type == "response" ~ eta * (-.3 + .2 * x)
    ) + .5 * x + b + rnorm(nrow(.), sd = .1),
    response = as.numeric(type == "response")
  ) %>%
  select(-eta, -b) %>%
  select(id, type, x, y, response) %>%
  as.data.frame()

usethis::use_data(latent_covariates_long, overwrite = TRUE)
