library(Matrix)
library(lme4)
library(memoise)

lmod <- lFormula(y ~ x + (0 + itemgroup | id), data = mresp)

theta_inds <- seq_along(lmod$reTrms$theta)
beta_inds <- seq(from = max(theta_inds) + 1, length.out = ncol(lmod$X))

mlwrapper <- function(par, hessian = FALSE){
  marginal_likelihood(
    y = mresp$y,
    trials = rep(1, nrow(mresp)),
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = lmod$reTrms$Lind - 1L,
    family = c("gaussian", "binomial"),
    family_mapping = ifelse(mresp$itemgroup == "a", 0L, 1L),
    maxit_conditional_modes = 10,
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

par_init <- c(lmod$reTrms$theta, rep(.3, ncol(lmod$X)))
lbound <- c(lmod$reTrms$lower, rep(-Inf, ncol(lmod$X)))
opt <- optim(par_init, fn, gr, method = "L-BFGS-B",
             lower = lbound, control = list(fnscale = -1))

fmod <- mlmem(opt$par, TRUE)

test_that("mixed response works", {
  expect_equal(opt$par, c(0.957958409447422, 1.06067106767555, 0.285720962425644, 0.0405115541598134,
                          0.974239048611714))
  expect_equal(opt$value, -4619.2040054477)
  expect_equal(fmod$phi, c(1.13721269186597, 1))
  expect_equal(fmod$hessian, structure(c(-411.301473824064, 3.38190917660314, -26.5042352287512,
                                         -6.51816420109335, 0.0758878337386766, 3.38190917660314, -157.513126198859,
                                         0.802903583741649, 12.0599363512705, 10.6712919771985, -26.5042352287512,
                                         0.802903583741649, -14.9600957042028, 4.29598831235914, 5.33248758760574,
                                         -6.51816420109335, 12.0599363512705, 4.29598831235914, -645.031086277487,
                                         -320.701676050362, 0.0758878337386766, 10.6712919771985, 5.33248758760574,
                                         -320.701676050362, -292.801074301447), dim = c(5L, 5L)))
})
