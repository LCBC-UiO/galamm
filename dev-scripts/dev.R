devtools::load_all()

library(lme4)
fm1 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy, REML = FALSE)

formula <- Reaction ~ Days + (1 | Subject)
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
stopifnot(length(increment) == 1)
y <- as.numeric(data[[all.vars(formula)[[1]]]])

(obj <- compute_galamm(
  y0 = y, X0 = X, Zt0 = ranef_obj$Zt, Lambdat0 = ranef_obj$Lambdat,
  Lind = ranef_obj$Lind - 1L
))


plot(obj$u * 36.012, ranef(fm1)$Subject$`(Intercept)`)
abline(0,1)
