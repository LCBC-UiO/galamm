devtools::load_all()

library(lme4)
library(tidyverse)

set.seed(100)
data <- tibble(
  id = 1:100, b = rnorm(100, sd = .1)
) %>%
  uncount(10) %>%
  mutate(
    x = runif(nrow(.)),
    y = rpois(nrow(.), lambda = exp(.1 * x + b))
    )

formula <- y ~ x + (1 | id)

gm1 <- glmer(formula, data = data, family = poisson, nAGQ = 0L)

logLik(gm1)

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



Zt <- getME(gm1, "Zt")
Lambdat <- getME(gm1, "Lambdat")
theta_init <- log(getME(gm1, "theta")) + rnorm(1, sd = .1)
theta_log <- 1L

obj <- galamm:::compute_galamm(
  y = as.numeric(y) , X = X, Zt = Zt, Lambdat = Lambdat, Lind = ranef_obj$Lind - 1L,
  theta = theta_init, theta_log = theta_log,
  maxit_outer = 10, family = "poisson", trials = rep(1, length(y)))

- obj$deviance / 2
logLik(gm1)

plot(obj$u, getME(gm1, "u")); abline(0, 1)
plot(obj$beta, fixef(gm1)); abline(0, 1)

