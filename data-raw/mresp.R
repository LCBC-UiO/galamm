# Basic mixed response dataset
library(tidyverse)
mresp <- tibble(id = 1:100) %>%
  mutate(b = rnorm(nrow(.))) %>%
  uncount(10, .id = "item") %>%
  mutate(
    trials = 10,
    y = if_else(item %in% 1:5, b + rnorm(nrow(.), sd = .1),
                rbinom(nrow(.), trials, plogis(1 + b)))
    ) %>%
  select(-b) %>%
  as.data.frame()

usethis::use_data(mresp, overwrite = TRUE)
