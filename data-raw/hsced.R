library(tidyverse)

set.seed(11)
n <- 200
hsced <- tibble(
  id = 1:n,
  b = rnorm(n)
) %>%
  uncount(3, .id = "tp") %>%
  uncount(2, .id = "item") %>%
  mutate(
    x = runif(nrow(.)),
    winv = if_else(item == 1, 1, 2),
    y = x + b + rnorm(nrow(.), sd = sqrt(winv))
  ) %>%
  select(-b, -winv) %>%
  as.data.frame()

usethis::use_data(hsced, overwrite = TRUE)
