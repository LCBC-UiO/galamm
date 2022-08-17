devtools::load_all()
library(lme4)
# library(PLmixed)
#
# data("IRTsim") # Load the IRTsim data
#
# IRTsub <- IRTsim[IRTsim$item < 4, ] # Select items 1-3
# set.seed(12345)
# IRTsub <- IRTsub[sample(nrow(IRTsub), 300), ] # Randomly sample 300 responses
#
# irt.lam = c(1, NA, NA) # Specify the lambda matrix
#
# # Below, the # in front of family = binomial can be removed to change the response distribution
# # to binomial, where the default link function is logit.
#
# formula <- y ~ 0 + as.factor(item) + (0 + abil.sid |sid) +(0 + abil.sid |school)
# data <- IRTsub
# load_var <- "item"
# factor <- list("abil.sid")
# lambda <- list(irt.lam)
# lambda_init <- lapply(lambda, function(x) {
#   x[is.na(x)] <- runif(sum(is.na(x)), 1, 2)
#   x
# })
# for(i in seq_along(factor)){
#   for(j in seq_along(factor[[i]])){
#     eval(parse(text = paste0("data$", factor[[i]][[j]], "<- lambda_init[[", i, "]][data$",
#                              load_var, "]")))
#   }
# }
#
# # Mapping between elements of Zt and lambda
# lambda_mapping <- sapply(lmod$reTrms$Zt@x, function(x) which(x == lambda_init[[1]]))
#


lmod <- lFormula(Reaction ~ Days + (1|Subject), sleepstudy, REML = FALSE)
devfun <- do.call(mkLmerDevfun, lmod)
opt <- optimizeLmer(devfun)
mm <- mkMerMod(environment(devfun), opt, lmod$reTrms, fr = lmod$fr)

mm2 <- compute_galamm(
  y = sleepstudy$Reaction,
  trials = rep(1, length(sleepstudy$Reaction)),
  X = lmod$X,
  Zt = lmod$reTrms$Zt,
  Lambdat = lmod$reTrms$Lambdat,
  beta = fixef(mm),
  theta = getME(mm, "theta"),
  theta_mapping = lmod$reTrms$Lind,
  lambda = 1,
  lambda_mapping_X = integer(),
  lambda_mapping_Zt = integer(),
  family = "gaussian"
  )

plot(getME(mm, "u"), mm2$u)



