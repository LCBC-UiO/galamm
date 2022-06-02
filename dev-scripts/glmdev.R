devtools::load_all()

library(lme4)
formula <- cbind(incidence, size - incidence) ~ period + (1 | herd)
gm1 <- glmer(formula, data = cbpp, family = binomial)

data <- cbpp
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
beta_init <- getME(gm1, "beta")
theta_init <- getME(gm1, "theta")
obj <- compute_galamm(y = y, X = X, Z = t(ranef_obj$Zt),
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
