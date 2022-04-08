library(tidyverse)

# Latent response dataset
# Binomial outcome
n3 <- 5
n2 <- 4
n1 <- 3
lambda <- c(item1 = 1, item2 = .4, items = 2)
beta <- c(0, .1) # intercept, time
trials <- 5

latent_response_dat <- tibble(
  id = seq_len(n3),
  b3 = rnorm(n3)
) %>%
  uncount(n2, .id = "tp") %>%
  mutate(
    time = runif(nrow(.), min = tp - 1L, max = tp),
    b2 = beta[[1]] + beta[[2]] * time + b3 + rnorm(nrow(.))
  ) %>%
  uncount(n1, .id = "item") %>%
  mutate(
    linpred = lambda[item] * b2,
    y = rbinom(nrow(.), size = trials, prob = plogis(linpred))
  ) %>%
  select(id, tp, item, time, y)

saveRDS(latent_response_dat, "inst/testdata/latent_response_dat.rds")
