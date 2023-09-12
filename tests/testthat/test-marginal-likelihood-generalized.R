library(memoise)
library(lme4)
data(cbpp)
glmod <- glFormula(cbind(incidence, size - incidence) ~ period + (1 | herd),
  data = cbpp, family = binomial
)

theta_inds <- 1
beta_inds <- 2:5
Lambdat <- glmod$reTrms$Lambdat

mlwrapper <- function(par, hessian = FALSE) {
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
fn <- function(par) {
  mlmem(par)$logLik
}
gr <- function(par) {
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
  expect_equal(opt$par, c(
    0.642260913490945, -1.39853183047491, -0.992333036156882, -1.12867162590934,
    -1.58031336794028
  ), tolerance = 1e-4)
  expect_equal(opt$value, -92.026284129683, tolerance = 1e-4)
  expect_equal(opt$convergence, 0)
  expect_equal(final_model$hessian, structure(c(
    -32.3851312969134, -3.51068800537797, 0.619024744034724,
    0.432263575757924, 0.397611061018501, -3.51068800537797, -24.0277124810337,
    -4.67476643363302, -3.91455545309, -1.99179774831731, 0.619024744034724,
    -4.67476643363302, -12.8229030840246, 1.93980201287065, 0.977067574679402,
    0.432263575757924, -3.91455545309, 1.93980201287065, -11.0527387991406,
    0.795541672068818, 0.397611061018501, -1.99179774831731, 0.977067574679402,
    0.795541672068818, -6.03673215094414
  ), dim = c(5L, 5L)), tolerance = 1e-4)
  expect_equal(final_model$u, c(
    0.91866064714488, -0.465382956525393, 0.632539155421036, 0.0611543940102624,
    -0.295854051641299, -0.623217987109506, 1.38479196038637, 0.933221232079885,
    -0.370028528730888, -0.842234822036188, -0.131779630554482, -0.100919537727657,
    -1.07420575068253, 1.51140492225922, -0.825950799579577
  ), tolerance = 1e-4)
})
