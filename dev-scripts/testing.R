library(lme4)
glmod <- glFormula(cbind(incidence, size - incidence) ~ period + (1 | herd),
                   data = cbpp, family = binomial)
devfun <- do.call(mkGlmerDevfun, glmod)
opt1 <- optimizeGlmer(devfun)
devfun <- updateGlmerDevfun(devfun, glmod$reTrms)
opt2 <- optimizeGlmer(devfun, stage=2)
fMod <- mkMerMod(environment(devfun), opt2, glmod$reTrms, fr = glmod$fr)

theta_inds <- 1
beta_inds <- 2:5
glmod$reTrms$Lambdat@x <- rep(1, 15)


marginal_likelihood(
  y = cbpp$incidence,
  trials = cbpp$size,
  X = glmod$X,
  Zt = glmod$reTrms$Zt,
  Lambdat = glmod$reTrms$Lambdat,
  beta = opt2$par[beta_inds],
  theta = opt2$par[theta_inds],
  theta_mapping = glmod$reTrms$Lind - 1L,
  lambda = numeric(),
  lambda_mapping_X = integer(),
  lambda_mapping_Zt = integer(),
  u = rep(0, nrow(glmod$reTrms$Zt)),
  family = "binomial",
  maxit_conditional_modes = 50
)




