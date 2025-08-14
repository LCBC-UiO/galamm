
library(tidyverse)
set.seed(123)
gamma <- c(1, 2)
lambda <- c(1, .5, 2, 1, .5, 2)
data <- tibble(id = seq_len(500)) %>%
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

lmat <- matrix(c(1, NA, NA, 0, 0, 0,
                 0, 0, 0, 1, NA, NA), ncol = 2)

mod <- galamm(
  formula = y ~ 0 + x:domain1:lambda1 + x:domain2:lambda2 +
    (0 + domain1:lambda1 + domain2:lambda2 | id),
  data = data,
  load_var = "item",
  lambda = list(lmat),
  factor = list(c("lambda1", "lambda2"))
  )

plot_smooth(mod, scale = 0, pages = 1)
