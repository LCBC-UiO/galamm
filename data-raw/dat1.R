## code to prepare `dat1` dataset goes here
library(tidyverse)
library(mgcv)
# Three items measuring a single underlying trait
n <- 100
lambda <- c(1, 2, .4)
set.seed(9987)
dat1 <- gamSim(n = n, verbose = FALSE) %>%
  select(x2, y) %>%
  rename(x = x2, eta = y) %>%
  mutate(
    id = row_number(),
    item1 = lambda[[1]] * eta + rnorm(n, sd = .3),
    item2 = lambda[[2]] * eta + rnorm(n, sd = .3),
    item3 = lambda[[3]] * eta + rnorm(n, sd = .3)
  ) %>%
  select(-eta) %>%
  pivot_longer(cols = c(item1, item2, item3), names_to = "item", values_to = "y") %>%
  select(id, item, y, x) %>%
  as.data.frame()

usethis::use_data(dat1, overwrite = TRUE)
