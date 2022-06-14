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

obj <- galamm:::compute_galamm(
  y = y, X = X, Zt = Zt, Lambdat = Lambdat, Lind = ranef_obj$Lind - 1L,
  theta = getME(fm1, "theta"))


plot(obj$beta, fixef(fm1)); abline(0, 1)

plot(obj$b, getME(fm1, "b")); abline(0,1)

