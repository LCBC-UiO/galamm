library(Matrix)
library(lme4)
library(memoise)
glmod <- glFormula(cbind(incidence, size - incidence) ~ period + (1 | herd),
                   data = cbpp, family = binomial)

theta_inds <- 1
beta_inds <- 2:5
Lambdat <- glmod$reTrms$Lambdat

mlwrapper <- function(par, hessian = FALSE){
  marginal_likelihood(
    y = cbpp$incidence,
    trials = cbpp$size,
    X = glmod$X,
    Zt = glmod$reTrms$Zt,
    Lambdat = Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = glmod$reTrms$Lind - 1L,
    family = "binomial",
    maxit_conditional_modes = 50,
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

set.seed(123)
opt <- optim(
  par = c(1, runif(4)), fn = fn,
  method = "L-BFGS-B", lower = c(0, rep(-Inf, 4)),
  control = list(fnscale = -1)
)
final_model <- mlwrapper(opt$par, hessian = TRUE)

test_that("ML GLMM works", {
  expect_equal(opt$par, c(0.642260913490945, -1.39853183047491, -0.992333036156882, -1.12867162590934,
                          -1.58031336794028), tolerance = 1e-4)
  expect_equal(opt$value, -92.026284129683, tolerance = 1e-4)
  expect_equal(opt$convergence, 0)
  expect_equal(final_model$hessian, structure(c(-24.027712480738, -4.67476643359328, -3.91455545303636,
                                                -1.99179774828282, -4.67476643359328, -12.8229030840504, 1.93980201288544,
                                                0.977067574683349, -3.91455545303636, 1.93980201288544, -11.0527387991183,
                                                0.79554167206802, -1.99179774828282, 0.977067574683349, 0.79554167206802,
                                                -6.03673215091366), dim = c(4L, 4L)), tolerance = 1e-4)
  expect_equal(final_model$u, c(0.91866064714488, -0.465382956525393, 0.632539155421036, 0.0611543940102624,
                                -0.295854051641299, -0.623217987109506, 1.38479196038637, 0.933221232079885,
                                -0.370028528730888, -0.842234822036188, -0.131779630554482, -0.100919537727657,
                                -1.07420575068253, 1.51140492225922, -0.825950799579577), tolerance = 1e-4)
})

