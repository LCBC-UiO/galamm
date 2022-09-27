library(nlme)
library(tidyverse)
set.seed(123)
n <- 200
dat <- crossing(
  subject = 1:n,
  item = factor(1:2)
) %>%
  mutate(
    x = runif(nrow(.)),
    y = x + rnorm(nrow(.), sd = if_else(item == 1, 1, .5))
  )

gd <- groupedData(y ~ 1 | item, data = dat)

mod <- lme(y ~ x, data = gd, weights = varIdent(form =~ 1 | item))

summary(mod)
sigma(mod)
