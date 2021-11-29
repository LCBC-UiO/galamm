## code to prepare `dat2` dataset goes here
library(tidyverse)

n2 <- 300
n1 <- 6

set.seed(1)
dat2 <- tibble(
  id = seq_len(n2), eta20 = rnorm(n2),
  eta21 = rnorm(n2),
  trials = 1
) %>%
  uncount(n1) %>%
  mutate(
    x = rnorm(nrow(.)),
    y = rbinom(nrow(.), size = trials, prob = plogis(.1 * x + eta20 + eta21 * x))
    ) %>%
  select(id, y, trials, x)

usethis::use_data(dat2, overwrite = TRUE)
