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
    lambda = numeric(),
    lambda_mapping_X = integer(),
    lambda_mapping_Zt = integer(),
    weights = numeric(),
    weights_mapping = integer(),
    family = "binomial",
    family_mapping = rep(0L, nrow(cbpp)),
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
  control = list(fnscale = -1, maxit = 2000, lmm = 20)
)
final_model <- mlwrapper(opt$par, hessian = TRUE)

test_that("ML GLMM works", {
  expect_equal(opt$par, c(0.642260913490945, -1.39853183047491, -0.992333036156882, -1.12867162590934,
                          -1.58031336794028))
  expect_equal(opt$value, -92.026284129683)
  expect_equal(opt$convergence, 0)
  expect_equal(final_model$hessian, structure(c(-32.3829330251185, -3.51131248271432, 0.618822481613185,
                                                0.432089101912972, 0.397518686815166, -3.51131248271432, -24.0266432049739,
                                                -4.67456324295464, -3.91435316736417, -1.99171389708833, 0.618822481613185,
                                                -4.67456324295464, -12.8227935981145, 1.93981127296748, 0.977081036248826,
                                                0.432089101912972, -3.91435316736417, 1.93981127296748, -11.0525757204985,
                                                0.795546335663779, 0.397518686815166, -1.99171389708833, 0.977081036248826,
                                                0.795546335663779, -6.03670021303939), dim = c(5L, 5L)))
  expect_equal(final_model$u, c(0.91866064714488, -0.465382956525393, 0.632539155421036, 0.0611543940102624,
                                -0.295854051641299, -0.623217987109506, 1.38479196038637, 0.933221232079885,
                                -0.370028528730888, -0.842234822036188, -0.131779630554482, -0.100919537727657,
                                -1.07420575068253, 1.51140492225922, -0.825950799579577))
})

