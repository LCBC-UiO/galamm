devtools::load_all()

library(lme4)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML = FALSE)



getME(fm1, "devcomp")$cmp[["sigmaML"]]

### Exact log likelihood
-getME(fm1, "devcomp")$cmp[["ldL2"]] / 2-
  sum(residuals(fm1)^2) / 2 / sigma(fm1)^2 -
  getME(fm1, "N") / 2 * log(2 * pi * sigma(fm1)^2) -
  sum(getME(fm1, "u")^2) / 2 / getME(fm1, "sigma")^2

logLik(fm1)

formula <- Reaction ~ Days + (Days | Subject)
data <- sleepstudy
latent <- NULL
lambda <- NULL

latent_barlist <- lme4::findbars(latent)
factors <- find_factors(latent_barlist, data)
load_vars <- find_load_vars(latent_barlist, data, lambda)
lambda_init <- initialize_lambda(lambda)
datax <- add_latent_to_data(latent_barlist, factors, load_vars, data,
                            lambda_init)
X <- model.matrix(lme4::nobars(formula), data = datax)

fixed_mapping <- find_load_cols(factors, load_vars, colnames(X))
lambda_mapping <- lapply(names(lambda), function(x) as.integer(datax[[x]]))
ranef_obj <- lme4::mkReTrms(lme4::findbars(formula), datax)

ranef_mapping <- lapply(ranef_obj$cnms,
                        function(x) find_load_cols(factors, load_vars, x))

increment <- unique(diff(ranef_obj$Zt@p))

y <- as.numeric(data[[all.vars(formula)[[1]]]])

set.seed(1)
beta_init <- getME(fm1, "beta") * runif(2, .9 , 1.1)
theta_init <- getME(fm1, "theta") * getME(fm1, "sigma") * runif(3, .9, 1.1)
obj <- compute_galamm(y = y, rep(1L, length = length(y)), X = X, Z = t(ranef_obj$Zt),
                      Lambda = t(ranef_obj$Lambdat), Lind = ranef_obj$Lind - 1L,
                      theta = theta_init,  theta_inds = 0:2,
                      beta = beta_init, beta_inds = 3:4, 700)


plot(obj$beta, fixef(fm1)); abline(0,1)

getME(fm1, "devcomp")$cmp[["ldL2"]] / 2


plot(obj$Lambda %*% obj$u, getME(fm1, "b")); abline(0, 1)

plot(obj$beta, fixef(fm1)); abline(0,1)

obj$theta
getME(fm1, "theta") * sigma(fm1)

logLik(fm1)
obj$loglik

obj$phi
as.data.frame(VarCorr(fm1))
