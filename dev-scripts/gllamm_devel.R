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

lambda_mapping_Zt <- sapply(lmod$reTrms$Zt@x, function(x) which(x == lambda_init[[1]]))

mm2 <- compute_galamm(
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
  lambda_mapping_Zt = lambda_mapping_Zt - 1L,
  lambda_free = c(2L, 3L) - 1L,
  family = "gaussian"
  )

par <- c(10, 200, 50)
grad <- NULL

theta_inds <- 1L
beta_inds <- c(2L, 3L)

fn <- function(par){
  ret <- compute_galamm(
    y = sleepstudy$Reaction,
    trials = rep(1, length(sleepstudy$Reaction)),
    X = lmod$X,
    Zt = lmod$reTrms$Zt,
    Lambdat = lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = lmod$reTrms$Lind - 1L,
    lambda = 1,
    lambda_mapping_X = integer(),
    lambda_mapping_Zt = integer(),
    family = "gaussian"
  )
  grad <<- ret$gradient
  ret$deviance
}
gr <- function(par){
  grad
}

opt <- optim(par, fn = fn, gr = gr, method = "L-BFGS-B",
             lower = c(0, -Inf, -Inf))

mm2$deviance
-2 * logLik(mm)

mm2
