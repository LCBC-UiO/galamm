library(PLmixed)
test_that("LMM with simple factor works", {
  data("IRTsim") # Load the IRTsim data

  IRTsub <- IRTsim[IRTsim$item < 4, ] # Select items 1-3
  set.seed(12345)
  IRTsub <- IRTsub[sample(nrow(IRTsub), 300), ] # Randomly sample 300 responses

  IRTsub <- IRTsub[order(IRTsub$item), ] # Order by item
  irt.lam = c(1, NA, NA) # Specify the lambda matrix

  mod <- galamm(
    y ~ 0 + as.factor(item) + (0 + abil.sid |sid) +(0 + abil.sid |school),
    data = IRTsub, load.var = c("item"),
    factor = list(c("abil.sid")), lambda = list(irt.lam)
  )

  expect_equal(mod$loglik, -193.563337783604)
})
