library(galamm)
library(lme4)
library(memoise)

glmod <- glFormula(cbind(incidence, size - incidence) ~ period + (1 | herd),
                  data = cbpp, family = binomial)
devfun <- do.call(mkGlmerDevfun, glmod)
opt <- optimizeGlmer(devfun)
devfun <- updateGlmerDevfun(devfun, glmod$reTrms)
opt <- optimizeGlmer(devfun, stage=2)
fMod <- mkMerMod(environment(devfun), opt, glmod$reTrms, fr = glmod$fr)

ml <- marginal_likelihood(
  y = cbpp$incidence,
  trials = cbpp$size,
  X = glmod$X,
  Zt = glmod$reTrms$Zt,
  Lambdat = glmod$reTrms$Lambdat,
  beta = fixef(fMod),
  theta = getME(fMod, "theta"),
  theta_mapping = glmod$reTrms$Lind - 1L,
  lambda = numeric(),
  lambda_mapping_X = integer(),
  lambda_mapping_Zt = integer(),
  family = "binomial"
)

logLik(fMod) * (-2)
ml$deviance

theta_inds <- 1
beta_inds <- 2:5

mlwrapper <- function(par){
  marginal_likelihood(
    y = cbpp$incidence,
    trials = cbpp$size,
    X = glmod$X,
    Zt = glmod$reTrms$Zt,
    Lambdat = glmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = glmod$reTrms$Lind - 1L,
    lambda = numeric(),
    lambda_mapping_X = integer(),
    lambda_mapping_Zt = integer(),
    family = "binomial"
  )
}

mlmem <- memoise(mlwrapper)
fn <- function(par){
  mlmem(par)$deviance
}
gr <- function(par){
  mlmem(par)$gradient
}

opt <- optim(
  par = c(1, rep(0, 4)), fn = fn, gr = gr,
  method = "L-BFGS-B", lower = c(0, rep(-Inf, 4))
)

-2 * logLik(fMod)
opt$value


opt$par
getME(fMod, "theta")
fixef(fMod)

library(PLmixed)

data("IRTsim") # Load the IRTsim data

IRTsub <- IRTsim[IRTsim$item < 4, ] # Select items 1-3
set.seed(12345)
IRTsub <- IRTsub[sample(nrow(IRTsub), 300), ] # Randomly sample 300 responses

irt.lam = c(1, NA, NA) # Specify the lambda matrix

# Below, the # in front of family = binomial can be removed to change the response distribution
# to binomial, where the default link function is logit.
formula <- y ~ 0 + as.factor(item) + (0 + abil.sid |sid) +(0 + abil.sid |school)
data <- IRTsub
load_var <- "item"
factor <- list("abil.sid")
lambda <- list(irt.lam)
lambda_init <- 1:3
data$abil.sid <- lambda_init[data$item]

mm <- glmer(formula, data, family = "binomial")
# Mapping between elements of Zt and lambda
lmod <- glFormula(formula, data = data, family = "binomial")

lambda_mapping_Zt <- (sapply(lmod$reTrms$Zt@x, function(x) which(x == lambda_init)) - 2L)

mm2 <- marginal_likelihood(
  y = data$y,
  trials = rep(1, nrow(data)),
  X = lmod$X,
  Zt = lmod$reTrms$Zt,
  Lambdat = lmod$reTrms$Lambdat,
  beta = fixef(mm),
  theta = getME(mm, "theta"),
  theta_mapping = lmod$reTrms$Lind - 1L,
  lambda = as.numeric(lambda_init[-1]),
  lambda_mapping_X = integer(),
  lambda_mapping_Zt = lambda_mapping_Zt,
  family = "binomial"
  )

mm2$deviance
-2 * logLik(mm)

par <- c(.2, .2, .6, .6, .6, 1, 1)
grad <- NULL

theta_inds <- c(1L, 2L)
beta_inds <- c(3L, 4L, 5L)
lambda_inds <- c(6L, 7L)
lower = c(0, 0, -Inf, -Inf, -Inf, -Inf, -Inf)

fn <- function(par){
  ret <- marginal_likelihood(
    y = data$y,
    trials = rep(1, nrow(data)),
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = lmod$reTrms$Lind - 1L,
    lambda = par[lambda_inds],
    lambda_mapping_X = integer(),
    lambda_mapping_Zt = lambda_mapping_Zt,
    family = "binomial"
  )
  grad <<- ret$gradient
  ret$deviance
}
gr <- function(par){
  grad
}

opt <- optim(par, fn = fn, gr = gr, method = "L-BFGS-B",
             lower = lower)

mm3 <- PLmixed(formula, IRTsub, load.var = load_var, lambda = list(irt.lam),
               factor = list("abil.sid"), family = "binomial", REML = FALSE)


summary(mm3)

mm2$deviance
-2 * logLik(mm)
-2 * mm3$`Log-Likelihood`

mm2
