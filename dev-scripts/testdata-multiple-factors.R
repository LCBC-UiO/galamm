rm(list=ls())
devtools::load_all()

data <- JUDGEsim
data$item <- factor(data$item)

lambda <- list(rbind(
  c(1, 0, 1, 0, 0, 0),
  c(NA, 0, NA, 0, 0, 0),
  c(NA, 0, NA, 0, 0, 0),
  c(0, 1, 0, 1, 0, 0),
  c(0, NA, 0, NA, 0, 0),
  c(0, NA, 0, NA, 0, 0),
  c(0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, NA, 0),
  c(0, 0, 0, 0, NA, 0),
  c(0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 0, 0, NA),
  c(0, 0, 0, 0, 0, NA)
))

factor <- list(c(
  "teacher1", "teacher2", "trait1.t",
  "trait2.t", "trait1.s", "trait2.s"
))

formula <- response ~ 0 + item + (0 + teacher1 + teacher2 | tch) +
  (0 + trait1.t + trait2.t + trait1.s + trait2.s | stu) +
  (1 | class)

load_var = "item"

weights = NULL
family = gaussian
family_mapping = rep(1L, nrow(data))

factor_interactions = NULL
start = NULL
control = galamm_control()
