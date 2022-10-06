library(Matrix)
library(lme4)
library(memoise)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

set.seed(100)
dat <- tibble(id = 1:1000) %>%
  mutate(b = rnorm(nrow(.))) %>%
  uncount(4, .id = "item") %>%
  mutate(
    x = runif(nrow(.)),
    y = pmap_dbl(
      list(b, item, x), ~ if_else(..2 %in% c(1, 2), rnorm(1, ..3 + ..1), as.numeric(rbinom(1, 1, plogis(..3 + ..1))))),
    itemgroup = if_else(item %in% c(1, 2), "a", "b")
  )

lmod <- lFormula(y ~ x + (0 + itemgroup | id), data = dat)

theta_inds <- seq_along(lmod$reTrms$theta)
beta_inds <- seq(from = max(theta_inds) + 1, length.out = ncol(lmod$X))

mlwrapper <- function(par, hessian = FALSE, epsilon_u = .1){
  marginal_likelihood(
    y = dat$y,
    trials = rep(1, nrow(dat)),
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = lmod$reTrms$Lind - 1L,
    family = c("gaussian", "binomial"),
    family_mapping = if_else(dat$item %in% c(1, 2), 0L, 1L),
    maxit_conditional_modes = 10,
    hessian = hessian,
    epsilon_u = epsilon_u
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par, epsilon_u = .1){
  mlmem(par, epsilon_u = epsilon_u)$logLik
}
gr <- function(par, epsilon_u = .1){
  mlmem(par, epsilon_u = epsilon_u)$gradient
}

par_init <- c(lmod$reTrms$theta, rep(.3, ncol(lmod$X)))
lbound <- c(lmod$reTrms$lower, rep(-Inf, ncol(lmod$X)))
opt <- optim(par_init, fn, gr, epsilon_u = 1e-5, method = "L-BFGS-B",
             lower = lbound, control = list(fnscale = -1))

fmod <- mlmem(opt$par, TRUE)


test_that("mixed response works", {
  expect_equal(opt$par, c(0.957958409447422, 1.06067106767555, 0.285720962425644, 0.0405115541598134,
                          0.974239048611714))
  expect_equal(opt$value, -4619.2040054477)
  expect_equal(fmod$phi, c(1.13720882645928, 1))
  expect_equal(fmod$hessian, structure(c(-411.353084880177, 3.4458649094352, -26.472360587959,
                                         -6.530760197769, 0.0684283841271554, 3.4458649094352, -157.58230702932,
                                         0.765762636440734, 12.0754780421646, 10.6803963000759, -26.472360587959,
                                         0.765762636440734, -15.0026336279258, 4.30449020846456, 5.33747040748976,
                                         -6.530760197769, 12.0754780421646, 4.30449020846456, -645.035172590591,
                                         -320.704973053981, 0.0684283841271554, 10.6803963000759, 5.33747040748976,
                                         -320.704973053981, -292.803601488173), dim = c(5L, 5L)))
})
