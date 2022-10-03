library(Matrix)
library(lme4)
library(memoise)
library(purrr, warn.conflicts = FALSE)

set.seed(100)
dat <- tibble(id = 1:1000) %>%
  mutate(b = rnorm(nrow(.))) %>%
  uncount(4, .id = "item") %>%
  mutate(
    y = map2_dbl(
      b, item, ~ if_else(.y %in% c(1, 2), rnorm(1, .x), as.numeric(rbinom(1, 1, plogis(.x)))))
  )

lmod <- lFormula(y ~ (1 | id), data = dat)

theta_inds <- 1
beta_inds <- 2

mlwrapper <- function(par, hessian = FALSE){
  marginal_likelihood(
    y = dat$y,
    trials = rep(1, nrow(dat)),
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = lmod$reTrms$Lind - 1L,
    lambda = numeric(),
    lambda_mapping_X = integer(),
    lambda_mapping_Zt = integer(),
    weights = numeric(),
    weights_mapping = integer(),
    family = c("gaussian", "binomial"),
    family_mapping = if_else(dat$item %in% c(1, 2), 0L, 1L),
    maxit_conditional_modes = 1,
    hessian = hessian
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par){
  mlmem(par)$logLik
}
gr <- function(par){
  mlmem(par)$gradient
}
opt <- optim(c(1, 0), fn, gr, method = "L-BFGS-B",
             lower = c(0, -Inf), control = list(fnscale = -1))

fmod <- mlmem(opt$par, TRUE)

test_that("mixed response works", {
  expect_equal(opt$par, c(0.954250269190533, 0.011596500858636))
  expect_equal(opt$value, -7634.42849692686)
  expect_equal(fmod$phi, 0.920149570635862)
  expect_equal(fmod$hessian, structure(c(-616.442272406721, -1.40589348444479, -1.40589348444479,
                                         -683.517659772292), dim = c(2L, 2L)))
})



