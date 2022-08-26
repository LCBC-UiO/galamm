library(lme4)
library(galamm)

fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML = FALSE)

lmod <- lFormula(Reaction ~ Days + (Days | Subject), sleepstudy)
devfun <- do.call(mkLmerDevfun, lmod)
opt <- optimizeLmer(devfun, control = list(method = "L-BFGS-B"))

fm2 <- marginal_likelihood(
  y = sleepstudy$Reaction,
  trials = rep(0, nrow(sleepstudy)),
  X = lmod$X,
  Zt = lmod$reTrms$Zt,
  Lambdat = lmod$reTrms$Lambdat,
  beta = fixef(fm1),
  theta = getME(fm1, "theta"),
  theta_mapping = lmod$reTrms$Lind - 1L,
  lambda = numeric(),
  lambda_mapping_X = integer(),
  lambda_mapping_Zt = integer(),
  family = "gaussian",
  maxit_conditional_modes = 1,
  compute_hessian = TRUE
)

logLik(fm1)
fm2$logLik

RX <- getME(fm1, "RX")
sigma2 <- sigma(fm1)^2
sigma2 * chol2inv(RX)

vcov(fm1)
sqrt(diag(-solve(fm2$hessian)))

newdat <- sleepstudy

betas <- sapply(simulate(fm1, nsim = 100), function(y){
  newdat$Reaction <- y
  fit <- lmer(Reaction ~ Days + (Days | Subject), newdat, REML = FALSE)
  fixef(fit)[["(Intercept)"]]
})
sd(betas)
