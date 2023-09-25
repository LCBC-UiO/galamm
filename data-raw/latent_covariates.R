library(tidyverse)

n <- 200

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
