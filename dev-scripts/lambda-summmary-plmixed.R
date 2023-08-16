library(PLmixed)
library(tidyverse)

data0 <- JUDGEsim[JUDGEsim$item %in% c(1:2, 4:5), ]
data0$item <- factor(data0$item)

# Simulate new response with low noise and strong effects
lambda_true <- rbind(
  c( 1,  0,  1,  0),
  c( 3,  0,  1,  0),
  c( 0,  1,  0,  1),
  c( 0, .1,  0,  3))

set.seed(1)
# Create simulated dataset
data <- data0 %>%
  mutate(item = factor(as.integer(item))) %>%
  nest_by(class) %>%
  ungroup() %>%
  mutate(class_intercept = rnorm(nrow(.))) %>%
  unnest(cols = c(data)) %>%
  nest_by(stu) %>%
  ungroup() %>%
  mutate(stu_intercept = rnorm(nrow(.))) %>%
  unnest(cols = c(data)) %>%
  nest_by(tch) %>%
  ungroup() %>%
  mutate(tch_intercept = rnorm(nrow(.))) %>%
  unnest(cols = c(data)) %>%
  mutate(
    teacher_lambda_ind = as.integer(item) + if_else(item %in% 1:2, 0L, 4L),
    teacher_lambda = lambda_true[teacher_lambda_ind],
    trait_lambda_ind = as.integer(item) + if_else(item %in% 1:2, 8L, 12L),
    trait_lambda = lambda_true[trait_lambda_ind],
    response = teacher_lambda * tch_intercept + trait_lambda * stu_intercept +
      class_intercept + rnorm(nrow(.), sd = .1)
  ) %>%
  as.data.frame()

lambda <- list(rbind(c( 1,  0,  1,  0),
                     c(NA,  0, NA, 0),
                     c( 0,  1,  0, 1),
                     c( 0, NA,  0, NA)))

formula <- response ~ 0 + item + (1 | class) + (0 + trait1.t + trait2.t | stu) + (0 + teacher1 + teacher2 | tch)
factor <- list(c("teacher1", "teacher2", "trait1.t", "trait2.t"))
load.var <- "item"

judge_plm <- PLmixed(
  formula = formula,
  data = data,
  lambda = lambda,
  load.var = load.var,
  factor = factor
)

# I get the following output Lambda:
lambda_est <- judge_plm$Lambda$lambda.item[, c(1, 3, 5, 7)]

# To save time, here is the structure without running the model above:
lambda_es <- structure(c(1, 2.99765944766908, 0, 0, 0, 0, 1, 0.999385071733127,
            1, 0.0994526598200496, 0, 0, 0, 0, 1, 2.99446923927171), dim = c(4L,
                                                                             4L), dimnames = list(c("1", "2", "3", "4"), c("teacher1", "teacher2",
                                                                                                                           "trait1.t", "trait2.t")))

# Compare to truth
# It seems like the Lambda output from PLmixed is filled by row rather than by column,
# and hence does not put the correct estimates into the right places
round(lambda_est, 2)
lambda_true

(lambda_est - lambda_true) / lambda_true

# The Param element seems to have the lambdas in the right order
judge_plm$Param
