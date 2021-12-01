## code to prepare `dat3` dataset goes here


## code to prepare `dat2` dataset goes here
library(tidyverse)

n2 <- 300
n1 <- 6

set.seed(1)
dat3 <- tibble(
  id = seq_len(n2), eta20 = rnorm(n2),
  eta21 = rnorm(n2),
  eta22 = rnorm(n2, sd = .5),
  trials = 1
) %>%
  uncount(n1) %>%
  mutate(
    x = rnorm(nrow(.)),
    y = rbinom(nrow(.), size = trials,
               prob = plogis(.1 * x + .1 * x^2 + eta20 + eta21 * x + eta22 * x^2))
  ) %>%
  select(id, y, trials, x)



usethis::use_data(dat3, overwrite = TRUE)
