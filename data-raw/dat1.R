## test dataset, simple linear mixed model
library(tidyverse)
set.seed(9987)
n <- 100
dat1 <- tibble(
  id = seq_len(n),
  tp = list(1:3),
  b = rnorm(n)
) %>%
  unnest(cols = tp) %>%
  mutate(y = .1 * tp + b + rnorm(nrow(.))) %>%
  select(-b) %>%
  as.data.frame()

usethis::use_data(dat1, overwrite = TRUE)
