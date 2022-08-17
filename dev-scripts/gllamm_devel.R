devtools::load_all()
library(lme4)
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
lambda_init <- lapply(lambda, function(x) {
  x[is.na(x)] <- seq(from = 2, length.out = sum(is.na(x)))
  x
})
for(i in seq_along(factor)){
  for(j in seq_along(factor[[i]])){
    eval(parse(text = paste0("data$", factor[[i]][[j]], "<- lambda_init[[", i, "]][data$",
                             load_var, "]")))
  }
}

mm <- lmer(formula, data, REML = FALSE)
# Mapping between elements of Zt and lambda
lmod <- lFormula(formula, data = data, REML = FALSE)

lambda_mapping_Zt <- (sapply(lmod$reTrms$Zt@x, function(x) which(x == lambda_init[[1]])) - 2L)


mm2 <- marginal_likelihood(
  y = data$y,
  trials = rep(1, nrow(data)),
  X = lmod$X,
  Zt = lmod$reTrms$Zt,
  Lambdat = lmod$reTrms$Lambdat,
  beta = fixef(mm),
  theta = getME(mm, "theta"),
  theta_mapping = lmod$reTrms$Lind - 1L,
  lambda = c(2, 3),
  lambda_mapping_X = integer(),
  lambda_mapping_Zt = lambda_mapping_Zt,
  family = "gaussian"
  )

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
    family = "gaussian"
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
               factor = list("abil.sid"), REML = FALSE)

summary(mm3)

mm2$deviance
-2 * logLik(mm)

mm2
