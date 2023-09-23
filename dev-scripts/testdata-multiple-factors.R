
library(tidyverse)
set.seed(123)
gamma <- c(1, 2)
lambda <- c(1, .5, 2, 1, .5, 2)
test_multiple_factors <- tibble(id = seq_len(500)) %>%
  uncount(2, .id = "domain") %>%
  mutate(b = rnorm(nrow(.))) %>%
  uncount(3, .id = "item") %>%
  mutate(item = factor(paste0(domain, item))) %>%
  mutate(
    x = runif(nrow(.)),
    y = b + gamma[domain] * x * lambda[item] + rnorm(nrow(.), sd = .1),
    domain1 = as.numeric(domain == 1),
    domain2 = as.numeric(domain == 2)
  )

saveRDS(test_multiple_factors, "inst/testdata/test_multiple_factors.rds")

