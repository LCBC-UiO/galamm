devtools::load_all()
library(PLmixed)
data("JUDGEsim")
JUDGEsim$item <- factor(JUDGEsim$item)
data <- JUDGEsim[JUDGEsim$item %in% 1:6, ]

judge.lam <- rbind(c( 1,  0),
                   c(NA,  0),
                   c(NA,  0),
                   c( 0,  1),
                   c( 0, NA),
                   c( 0, NA))

judge_galamm <- galamm(
  formula = response ~ 0 + item + (1 | class) + (0 + teacher1 + teacher2 | tch),
  data = data,
  lambda = list(judge.lam),
  load.var = "item",
  factor = list(c("teacher1", "teacher2"))
)


lambda <- list(rbind(c( 1,  0,  1),
                   c(NA,  0, NA),
                   c(NA,  0, NA),
                   c( 0,  1,  0),
                   c( 0, NA,  0),
                   c( 0, NA,  0)))

formula <- response ~ 0 + item + (1 | class) + (0 + trait1.t | stu) + (0 + teacher1 + teacher2 | tch)
factor <- list(c("teacher1", "teacher2", "trait1.t"))
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
  factor = factor
)

summary(judge_galamm)
summary(judge_plm)
