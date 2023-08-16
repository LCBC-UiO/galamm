devtools::load_all()
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
      class_intercept + rnorm(nrow(.), sd = .01)
  ) %>%
  as.data.frame()

lambda <- list(rbind(c( 1,  0,  1,  0),
                     c(NA,  0, NA, 0),
                     c( 0,  1,  0, 1),
                     c( 0, NA,  0, NA)))

formula <- response ~ 0 + item + (1 | class) + (0 + trait1.t + trait2.t | stu) + (0 + teacher1 + teacher2 | tch)
factor <- list(c("teacher1", "teacher2", "trait1.t", "trait2.t"))
load.var <- "item"

judge_galamm <- galamm(
  formula = formula,
  data = data,
  lambda = lambda,
  load.var = load.var,
  factor = factor
)

judge_plm <- PLmixed(
  formula = formula,
  data = data,
  lambda = lambda,
  load.var = load.var,
  factor = factor,
  opt.control = list(maxit = 2)
)


# In this vector they are in correct order
judge_plm$Param
judge_galamm$par[judge_galamm$lambda_inds]

# But in Lambda the estimates are spread by row and not by column
judge_plm$Lambda

judge_plm$Lambda$lambda.item[, factor[[1]]]
lambda_true



lambda_true <- rbind(
  c( 1,  0,  1,  0),
  c( 3,  0,  1,  0),
  c( 0,  1,  0,  1),
  c( 0, .1,  0,  3))

fill <- 1
num <- 4
col <- 4
uniq <- 1:4
factor.2 <- factor[[1]]
lambda.1 <- lambda[[1]]
Est <- c(3, .1, 1, 3)
st.er <- c(1, 1, 1, 1) * .1
final.lambda <- matrix(NA, nrow = num,
                       ncol = 2 * (length(factor.2)))
se.names <- rep("SE", length(factor.2))
comb <- rbind(factor.2, se.names)
comb <- matrix(comb, nrow = 1, ncol = 2 * length(factor.2),
               byrow = F)
rownames(final.lambda) <- uniq
colnames(final.lambda) <- comb
for (i in 1:nrow(as.matrix(lambda.1))) {
  for (j in 1:ncol(as.matrix(lambda.1))) {
    if (is.na(lambda.1[i, j]) == 1) {
      final.lambda[i, (j * 2 - 1)] <- Est[fill]
      final.lambda[i, j * 2] <- st.er[fill]
      fill <- fill + 1
    }
    else {
      final.lambda[i, (j * 2 - 1)] <- lambda.1[i, j]
    }
  }
}
fin.lam[[h]] <- final.lambda

lam.names <- paste0("lambda.", load.var)
names(fin.lam) <- lam.names
