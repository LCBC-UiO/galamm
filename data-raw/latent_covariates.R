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
latent_covariates_long <- latent_covariates %>%
  bind_rows(
    filter(latent_covariates, response == 1),
    filter(latent_covariates, response == 1),
    filter(latent_covariates, response == 1),
    filter(latent_covariates, response == 1),
    filter(latent_covariates, response == 1)
    ) %>%
  arrange(id) %>%
  mutate(
    y = case_when(
      response == 1 ~ y + rnorm(nrow(.), sd = .05),
      TRUE ~ y
    )
  )

usethis::use_data(latent_covariates_long, overwrite = TRUE)
