library(PLmixed)
library(Matrix)
data("IRTsim")
IRTsub <- IRTsim[IRTsim$item < 4, ]
set.seed(12345)
IRTsub <- IRTsub[sample(nrow(IRTsub), 300), ]
IRTsub <- IRTsub[order(IRTsub$item), ]
irt.lam = c(1, NA, NA)
form <- y ~ 0 + as.factor(item) + (0 + abil.sid |sid) +(0 + abil.sid |school)
dat <- IRTsub
dat$abil.sid <- 1
lmod <- lFormula(form, data = dat, REML = FALSE)
X <- lmod$X
Zt <- lmod$reTrms$Zt
table(diff(Zt@p))
lambda_mapping_Zt <- rep(dat$item, each = 2) - 2L
Lambdat <- lmod$reTrms$Lambdat
theta_mapping <- lmod$reTrms$Lind - 1L
ml <- marginal_likelihood(
  y = dat$y,
  trials = rep(1, length(dat$y)),
  X = X,
  Zt = Zt,
  Lambdat = Lambdat,
  beta = c(`as.factor(item)1` = 0.645688690888936, `as.factor(item)2` = 0.598327965143937,
           `as.factor(item)3` = 0.545950736623585),
  theta = c(sid.abil.sid = 0.634427344994445, school.abil.sid = 0.578028244846488),
  theta_mapping = theta_mapping,
  lambda = c(`2` = 1.05448883376966, `3` = 1.02127875663853),
  lambda_mapping_X = integer(),
  lambda_mapping_Zt = lambda_mapping_Zt,
  weights = numeric(),
  weights_mapping = integer(),
  family = "gaussian",
  maxit_conditional_modes = 1
)

test_that("ML Gaussian model works", {
  expect_equal(ml$logLik, -193.563337768878)
  expect_equal(ml$gradient, c(1.31400267378012e-06, -7.46553109109982e-07, -2.36088454505896e-13,
                              -1.16758056086726e-13, 3.58263321968292e-13, -1.93037701237131e-05,
                              1.57322876681576e-05))

})

library(memoise)
theta_inds <- 1:2
beta_inds <- 3:5
lambda_inds <- 6:7
bounds <- c(0, 0, rep(-Inf, 5))

mlwrapper <- function(par){
  marginal_likelihood(
    y = dat$y,
    trials = rep(1, length(dat$y)),
    X = X,
    Zt = Zt,
    Lambdat = Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = theta_mapping,
    lambda = par[lambda_inds],
    lambda_mapping_X = integer(),
    lambda_mapping_Zt = lambda_mapping_Zt,
    weights = numeric(),
    weights_mapping = integer(),
    family = "gaussian",
    maxit_conditional_modes = 1
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par){
  mlmem(par)$logLik
}
gr <- function(par){
  mlmem(par)$gradient
}

par_init <- c(1, 1, 0, 0, 0, 1, 1)
opt <- optim(par_init, fn = fn, gr = gr,
             method = "L-BFGS-B", lower = bounds,
             control = list(fnscale = -1))

test_that("marginal likelihood optimization works", {
  expect_equal(opt$par, c(0.634441650291438, 0.577989656676028, 0.645706065689437, 0.598347710112585,
                          0.545957767129257, 1.05457112220703, 1.02131277585108))
  expect_equal(opt$value, -193.563337950468)
  expect_equal(opt$convergence, 0)
})

final_model <- marginal_likelihood(
  y = dat$y,
  trials = rep(1, length(dat$y)),
  X = X,
  Zt = Zt,
  Lambdat = Lambdat,
  beta = opt$par[beta_inds],
  theta = opt$par[theta_inds],
  theta_mapping = theta_mapping,
  lambda = opt$par[lambda_inds],
  lambda_mapping_X = integer(),
  lambda_mapping_Zt = lambda_mapping_Zt,
  weights = numeric(),
  weights_mapping = integer(),
  family = "gaussian",
  maxit_conditional_modes = 1,
  hessian = TRUE
)
S <- solve(-final_model$hessian)

test_that("Hessian is correct", {
  expect_equal(S, structure(c(0.0357330529544457, 0.0107646140959335, 0.000593180001903165,
                              -0.000459635666847079, -0.000236102170500113, -0.0152811861614752,
                              -0.00895326878884668, 0.0107646140959335, 0.0208922267967086,
                              0.000176736985010183, -6.93661141540457e-05, 0.00018603459830411,
                              -0.0139131257999688, -0.0136636401630359, 0.000593180001903165,
                              0.000176736985010183, 0.00397737015835084, 0.00210817906501848,
                              0.00203996955582461, -0.000240958171965212, 0.000164984395667197,
                              -0.000459635666847079, -6.93661141540456e-05, 0.00210817906501848,
                              0.00386940746589562, 0.00215619446240762, 0.000475299654704952,
                              0.000288029013481691, -0.000236102170500113, 0.00018603459830411,
                              0.00203996955582461, 0.00215619446240762, 0.00400340054620927,
                              -6.65409259037329e-05, 0.000552379489896932, -0.0152811861614752,
                              -0.0139131257999688, -0.000240958171965212, 0.000475299654704952,
                              -6.65409259037327e-05, 0.0474823910699908, 0.0252733561893344,
                              -0.00895326878884668, -0.0136636401630359, 0.000164984395667197,
                              0.000288029013481692, 0.000552379489896932, 0.0252733561893344,
                              0.0560849371316741), dim = c(7L, 7L)))
})

