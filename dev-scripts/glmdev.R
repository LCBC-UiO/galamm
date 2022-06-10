
devtools::load_all()

library(lme4)
formula <- cbind(incidence, size - incidence) ~ period + (1 | herd)
gm1 <- glmer(formula, data = cbpp, family = binomial)

logLik(gm1)

dc <- getME(gm1, "devcomp")$cmp

- dc[["ldL2"]] / 2 +
sum(cbpp$incidence * predict(gm1, type = "link")) -
  sum(log(1 + exp(predict(gm1, type = "link"))) * cbpp$size) +
  sum(log(choose(cbpp$size, cbpp$incidence))) -
  dc[["ussq"]]/2

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
trials <- cbpp$size
obj <- compute_galamm(y = y, trials = trials, X = X,
                      Z = t(ranef_obj$Zt),
                      Lambda = t(ranef_obj$Lambdat), Lind = ranef_obj$Lind - 1L,
                      theta = theta_init,  theta_inds = 0L,
                      beta = beta_init, beta_inds = 1:4, 1, "binomial")


u <- matrix(0, ncol(obj$Lambda), 10)
for(i in 2:ncol(u)){
  u[, i] <- as.numeric(t(obj$Lambda) %*% obj$Lambda %*% ranef_obj$Zt %*% (y - plogis(as.numeric(X %*% obj$beta + t(ranef_obj$Zt) %*% u[, i - 1]))))
}



eta0 <- as.numeric(X %*% obj$beta + t(ranef_obj$Zt) %*% obj$Lambda %*% u0)
rhs0 <- t(obj$Lambda) %*% ranef_obj$Zt %*% (y - plogis(eta0)) - u0
H0 <- as.matrix(t(obj$Lambda) %*% ranef_obj$Zt %*% diag(x = as.numeric(trials * plogis(eta0) * (1 - plogis(eta0))))
                %*% t(ranef_obj$Zt) %*% obj$Lambda + diag(rep(1, ncol(obj$Lambda))) )
(u0 <- u0 + solve(H0, rhs0))

plot(u0, obj$u)

eta <- as.numeric(X %*% obj$beta + t(ranef_obj$Zt) %*% obj$Lambda %*% obj$u)



H <- t(obj$Lambda) %*% ranef_obj$Zt %*% diag(x = trials * plogis(eta) * (1 - plogis(eta))) %*% t(ranef_obj$Zt) %*% obj$Lambda + diag(rep(1, ncol(obj$Lambda)))

plot(diag(H), diag(obj$H)); abline(0, 1)

- determinant(obj$H)$modulus / 2 +
  sum(y * eta) -
  sum(log(1 + exp(eta)) * trials) +
  sum(log(choose(trials, y))) - sum(obj$u^2)/2


- dc[["ldL2"]] / 2 +
  sum(cbpp$incidence * (X %*% beta_init + t(ranef_obj$Zt) %*% getME(gm1, "b"))) -
  sum(log(1 + exp(predict(gm1, type = "link"))) * cbpp$size) +
  sum(log(choose(cbpp$size, cbpp$incidence))) -
  dc[["ussq"]]/2



sum(y * predict(gm1, type = "link", re.form = NULL))

V <- diag(trials * predict(gm1, type = "response", re.form = NULL) *
  (1 - predict(gm1, type = "response", re.form = NULL)))

plot(diag(t(obj$Lambda) %*% ranef_obj$Zt %*% V %*% t(ranef_obj$Zt) %*% obj$Lambda +
  diag(ncol(obj$Lambda))),
diag(obj$H))


plot(obj$Lambda %*% obj$u, ranef(gm1)$herd$`(Intercept)`); abline(0,1)

delta <- solve(obj$H, obj$g)
plot(u, getME(gm1, "u"))

plot(obj$V,
     trials * predict(gm1, type = "response", re.form = NULL) *
       (1 - predict(gm1, type = "response", re.form = NULL)))

plot(obj$eta, predict(gm1, type = "link", re.form = NULL))

sum(trials * log(1 + exp(predict(gm1, type = "link", re.form =NULL))))


lgamma(trials + 1) - lgamma(trials - y + 1) - lgamma(y + 1)
predict(gm1, type = "link")

plot(obj$beta, fixef(gm1)); abline(0,1)

getME(fm1, "devcomp")$cmp[["ldL2"]] / 2


plot(obj$Lambda %*% obj$u, getME(gm1, "b")); abline(0, 1)

plot(obj$beta, fixef(gm1)); abline(0,1)

obj$theta
getME(fm1, "theta") * sigma(fm1)

logLik(fm1)
obj$loglik

obj$phi
as.data.frame(VarCorr(fm1))
