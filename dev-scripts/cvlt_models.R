library(galamm)
library(gamm4)
library(memoise)
dat <- readRDS("~/code/galamm-scripts/data/cvlt_model/simulated_data.rds")
dat$trials <- dat$successes + dat$failures

lambda_init <- c(1, 1.63592557746164, 2.17794548382306, 2.3994772867059, 2.58913350262379)
names(lambda_init) <- paste0("a", 1:5)
dat$weight <- lambda_init[dat$item]
sm <- smoothCon(s(age_z, by = weight, k = 15, bs = "cr"), data = dat)[[1]]
re <- smooth2random(sm, "", 2)

ldat <- unclass(dat)
ldat$Xf <- re$Xf
ldat$Xr <- re$rand$Xr
ldat$pseudoGroups <- rep(1:ncol(re$rand$Xr), length = nrow(dat))

glmod <- glFormula(
  cbind(successes, failures) ~ 0 + a2 + a3 + a4 + a5 + retest +
    Xf + (1 | pseudoGroups) + (0 + weight | id / timepoint),
  data = ldat, family = binomial
)

glmod$reTrms$Ztlist$`1 | pseudoGroups` <-
  as(t(as.matrix(ldat$Xr)), class(glmod$reTrms$Zt))

glmod$reTrms$Zt <- rbind(glmod$reTrms$Ztlist$`0 + weight | timepoint:id`,
                         glmod$reTrms$Ztlist$`0 + weight | id`,
                         glmod$reTrms$Ztlist$`1 | pseudoGroups`)

# Columns 6 and 7 of X contain weights
head(glmod$X)
lambda_mapping_X <- c(rep(-1L, nrow(glmod$X) * 5),
                      rep(as.integer(as.factor(ldat$item)) - 2L, 2))
tail(glmod$X)
tail(lambda_mapping_X)

# All rows of Zt contain weights
# Each column of Zt has 15 entries, all connected to a single data point.
table(diff(glmod$reTrms$Zt@p))
lambda_mapping_Zt <- rep(as.integer(as.factor(ldat$item)) - 2L, each = 15)

# Now finish fitting the fixed loadings model
# devfun <- do.call(mkGlmerDevfun, glmod)
# opt <- optimizeGlmer(devfun)
# devfun <- updateGlmerDevfun(devfun, glmod$reTrms)
# opt <- optimizeGlmer(devfun, stage=2)
# fMod <- mkMerMod(environment(devfun), opt, glmod$reTrms, fr = glmod$fr)
# logLik(fMod)

par <- c(`timepoint:id.weight` = 0.401684867107717, id.weight = 0.432594808520555,
         `pseudoGroups.(Intercept)` = 2.21960459330373, a2 = 1.26247484991802,
         a3 = 2.1528236647247, a4 = 2.64812028864908, a5 = 3.10950593442185,
         retest = 0.225277517427026, Xf1 = -1.37251801415483, Xf2 = -0.19944733429615,
         1.63592557746164, 2.17794548382306, 2.3994772867059, 2.58913350262379
)

beta_inds <- 4:10
par[beta_inds]
theta_inds <- 1:3
par[theta_inds]
lambda_inds <- 11:14
par[lambda_inds]

marginal_likelihood(
  y = as.numeric(ldat$successes),
  trials = as.numeric(ldat$trials),
  X = glmod$X,
  Zt = glmod$reTrms$Zt,
  Lambdat = glmod$reTrms$Lambdat,
  beta = par[beta_inds],
  theta = par[theta_inds],
  theta_mapping = glmod$reTrms$Lind - 1L,
  lambda = par[lambda_inds],
  lambda_mapping_X = lambda_mapping_X,
  lambda_mapping_Zt = lambda_mapping_Zt,
  family = "binomial",
  5
)
