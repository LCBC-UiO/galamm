rm(list=ls())
devtools::load_all()
dat <- subset(
  cognition,
  domain %in% c(1, 3) & item %in% c("11", "12", "31", "32"))
dat <- cbind(
  dat,
  model.matrix(~ 0 + domain, data = dat)[, c("domain1", "domain3")]
)
lmat <- matrix(c(
  1, NA, 0, 0,
  0, 0, 1, NA
), ncol = 2)


formula = y ~
  domain + sl(x, k = 4, by = domain, load.var = c("ability1", "ability3")) +
  (0 + domain1:ability1 + domain3:ability3 | id)
weights=NULL
data = dat
family = gaussian
family_mapping = rep(1L, nrow(data))
load.var = "item"
lambda = list(lmat)
factor = list(c("ability1", "ability3"))
factor_interactions = NULL
start = NULL
control = galamm_control(
  optim_control = list(maxit = 0), reduced_hessian = TRUE
)

data <- stats::na.omit(data)
if (nrow(data) == 0) stop("No data, nothing to do.")
data <- as.data.frame(data)
mc <- match.call()

family_list <- setup_family(family)
if (!is.vector(family_mapping)) {
  stop("family_mapping must be a vector.")
}
if (is.numeric(family_mapping)) {
  family_mapping <- as.integer(family_mapping)
}

stopifnot(length(family_list) == length(unique(family_mapping)))

tmp <- setup_factor(load.var, lambda, factor, data)
data <- tmp$data
lambda_orig <- tmp$lambda
rm(tmp)

rf <- lme4::findbars(formula)
rf <- if (!is.null(rf)) {
  stats::as.formula(paste("~", paste("(", rf, ")", collapse = "+")))
}
gobj <- gamm4(fixed = lme4::nobars(formula), random = rf, data = data)
colnames(gobj$lmod$X) <- gsub("^X", "", colnames(gobj$lmod$X))

response_obj <-
  setup_response_object(family_list, family_mapping, data, gobj)
lambda_mappings <- define_factor_mappings(
  gobj, load.var, lambda_orig, factor, factor_interactions, data
)

lambda <- lambda_mappings$lambda

theta_mapping <- gobj$lmod$reTrms$Lind - 1L
theta_inds <- seq_along(gobj$lmod$reTrms$theta)
beta_inds <- max(theta_inds) + seq_along(colnames(gobj$lmod$X))
lambda_inds <- max(beta_inds) + seq_along(lambda[[1]][lambda[[1]] >= 2])

if (!is.null(weights)) {
  weights_obj <- lme4::mkReTrms(lme4::findbars(weights), fr = data)
  if (length(weights_obj$flist) > 1) {
    stop("Multiple grouping terms in weights not yet implemented.")
  }
  delta <- diff(weights_obj$Zt@p)
  weights_mapping <- as.integer(weights_obj$flist[[1]]) - 2L
  weights_mapping[delta == 0] <- -1L
  weights_inds <- length(unique(weights_mapping)) +
    max(c(theta_inds, beta_inds, lambda_inds)) - 1L
} else {
  weights_obj <- NULL
  weights_mapping <- integer()
  weights_inds <- integer()
}

bounds <- c(
  gobj$lmod$reTrms$lower,
  rep(-Inf, length(beta_inds) + length(lambda_inds)),
  rep(0, length(weights_inds))
)

y <- response_obj[, 1]
trials <- response_obj[, 2]
u_init <- rep(0, nrow(gobj$lmod$reTrms$Zt))
family_txt <- vapply(family_list, function(f) f$family, "a")
k <- find_k(family_txt, family_mapping, y, trials)

maxit_conditional_modes <- ifelse(
  length(family_list) == 1 & family_list[[1]]$family == "gaussian",
  1, control$maxit_conditional_modes
)

mlwrapper <- function(par, gradient = FALSE, hessian = FALSE) {
  marginal_likelihood(
    y = y,
    trials = trials,
    X = gobj$lmod$X,
    Zt = gobj$lmod$reTrms$Zt,
    Lambdat = gobj$lmod$reTrms$Lambdat,
    beta = par[beta_inds],
    theta = par[theta_inds],
    theta_mapping = theta_mapping,
    u_init = u_init,
    lambda = par[lambda_inds],
    lambda_mapping_X = lambda_mappings$lambda_mapping_X,
    lambda_mapping_Zt = lambda_mappings$lambda_mapping_Zt,
    lambda_mapping_Zt_covs = lambda_mappings$lambda_mapping_Zt_covs,
    weights = par[weights_inds],
    weights_mapping = weights_mapping,
    family = family_txt,
    family_mapping = family_mapping - 1L,
    k = k,
    maxit_conditional_modes = maxit_conditional_modes,
    lossvalue_tol = control$pwirls_tol_abs,
    gradient = gradient,
    hessian = hessian,
    reduced_hessian = control$reduced_hessian
  )
}

mlmem <- memoise::memoise(mlwrapper)
fn <- function(par, gradient, hessian = FALSE) {
  mlmem(par, gradient, hessian)$logLik
}
gr <- function(par, gradient, hessian = FALSE) {
  mlmem(par, gradient, hessian)$gradient
}

par_init <-
  set_initial_values(gobj, start, beta_inds, lambda_inds, weights_inds)

fn(par_init, F)
