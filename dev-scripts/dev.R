devtools::load_all()

library(lme4)
formula <- Reaction ~ Days + (Days | Subject)
fm1 <- lmer(formula, sleepstudy, REML = FALSE)


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


Zt <- ranef_obj$Zt
Lambdat <- ranef_obj$Lambdat

s <- 1
sigma <- .01
beta <- .5

theta <- getME(fm1, "theta")
theta_log <- as.integer(ranef_obj$lower == 0)
theta <- ifelse(theta_log == 1, log(theta), theta)

obj <- galamm:::compute_galamm(
  y = y, X = X, Zt = Zt, Lambdat = Lambdat, Lind = ranef_obj$Lind - 1L,
  theta = theta + rnorm(3, sd = .05),
  theta_log = theta_log,
  maxit_outer = 10,
  family = "gaussian", trials = rep(1, length(y)))



-obj$deviance / 2
logLik(fm1)


plot(obj$beta, fixef(fm1)); abline(0, 1)

plot(obj$b, getME(fm1, "b")); abline(0,1)

plot(obj$u, getME(fm1, "u")); abline(0,1)

