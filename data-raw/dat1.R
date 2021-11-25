## code to prepare `dat1` dataset goes here
library(tidyverse)

n2 <- 100
n1 <- 3

set.seed(1)
dat1 <- tibble(
  id = seq_len(n2), eta2 = rnorm(n2), trials = 1, x = rnorm(n2)
) %>%
  uncount(n1) %>%
  mutate(y = rbinom(nrow(.), size = trials, prob = plogis(.1 * x + eta2))) %>%
  select(id, y, trials, x)

usethis::use_data(dat1, overwrite = TRUE)
