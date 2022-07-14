devtools::load_all()

library(lme4)
library(tidyverse)

set.seed(100)
data <- tibble(
  id = 1:100, b = rnorm(100)
) %>%
  uncount(10) %>%
  mutate(
    x = runif(nrow(.)),
    trials = sample(3:7, size = nrow(.), replace = TRUE),
    y = rbinom(nrow(.), trials, prob = plogis(x + b))
    )

formula <- cbind(y, trials - y) ~ x + (1 | id)

gm1 <- glmer(formula, data = data, family = binomial, nAGQ = 0L)


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

theta_init <- getME(gm1, "theta") + rnorm(1, sd = .1)
theta_log <- 1L
theta_init <- log(theta_init)

Zt <- getME(gm1, "Zt")
Lambdat <- getME(gm1, "Lambdat")

obj <- galamm:::compute_galamm(
  y = as.numeric(y) , X = X, Zt = Zt, Lambdat = Lambdat, Lind = ranef_obj$Lind - 1L,
  theta = theta_init, theta_log = theta_log, maxit_outer = 30, family = "binomial",
  trials = as.numeric(data$trials))



plot(obj$u, getME(gm1, "u")); abline(0, 1)
plot(c(obj$beta, exp(obj$theta)), c(fixef(gm1), getME(gm1, "theta"))); abline(0, 1)


