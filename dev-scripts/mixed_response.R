devtools::load_all()
library(Matrix)
library(lme4)
library(dplyr)
library(tidyr)
library(purrr)

dat <- tibble(id = 1:100) %>%
  mutate(b = rnorm(nrow(.))) %>%
  uncount(4, .id = "item") %>%
  mutate(
    y = map2_dbl(b, item, ~ if_else(.y %in% c(1, 2), rnorm(1, .x),
                                    as.numeric(rbinom(1, 1, plogis(.x)))))
  )

lmod <- lFormula(y ~ (1 | item), data = dat)

margl <- marginal_likelihood(
  y = dat$y,
  trials = rep(1, nrow(dat)),
  X = lmod$X,
  Zt = lmod$reTrms$Zt,
  Lambdat = lmod$reTrms$Lambdat,
  beta = 0,
  theta = 1,
  theta_mapping = lmod$reTrms$Lind - 1L,
  lambda = numeric(),
  lambda_mapping_X = integer(),
  lambda_mapping_Zt = integer(),
  weights = numeric(),
  weights_mapping = integer(),
  family = c("gaussian", "binomial"),
  family_mapping = if_else(dat$item %in% c(1, 2), 0L, 1L),
  maxit_conditional_modes = 3,
  hessian = TRUE
)
