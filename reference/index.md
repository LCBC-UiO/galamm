# Package index

## Modeling

Fit a generalized additive latent and mixed model.

- [`galamm()`](https://lcbc-uio.github.io/galamm/reference/galamm.md) :
  Fit a generalized additive latent and mixed model
- [`galammObject`](https://lcbc-uio.github.io/galamm/reference/galammObject.md)
  : Class "galamm"
- [`gfam()`](https://lcbc-uio.github.io/galamm/reference/gfam.md) :
  Grouped families
- [`sl()`](https://lcbc-uio.github.io/galamm/reference/sl.md) : Set up
  smooth term with factor loading
- [`t2l()`](https://lcbc-uio.github.io/galamm/reference/t2l.md) : Set up
  smooth term with factor loading
- [`galamm-package`](https://lcbc-uio.github.io/galamm/reference/galamm-package.md)
  : galamm: Generalized Additive Latent and Mixed Models

## High-level model output

Functions for summarising the result of model fits.

- [`anova(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/anova.galamm.md)
  : Compare likelihoods of galamm objects
- [`plot_smooth(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/plot_smooth.galamm.md)
  : Plot smooth terms for galamm fits
- [`print(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/print.galamm.md)
  : Print method for GALAMM fits
- [`print(`*`<summary.galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/print.summary.galamm.md)
  : Print method for summary GALAMM fits
- [`summary(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/summary.galamm.md)
  : Summarizing GALAMM fits

## Diagnostics

Functions helping with model diagnostics.

- [`plot(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/plot.galamm.md)
  : Diagnostic plots for galamm objects
- [`qqmath(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/qqmath.galamm.md)
  : Quantile-quantile plots for galamm objects

## Model details

Detailed information on specific parts of model fits.

- [`VarCorr(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/VarCorr.md)
  : Extract variance and correlation components from model
- [`coef(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/coef.galamm.md)
  : Extract galamm coefficients
- [`confint(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/confint.galamm.md)
  : Confidence intervals for model parameters
- [`deviance(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/deviance.galamm.md)
  : Extract deviance of galamm object
- [`factor_loadings(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/factor_loadings.galamm.md)
  : Extract factor loadings from galamm object
- [`family(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/family.galamm.md)
  : Extract family or families from fitted galamm
- [`fitted(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/fitted.galamm.md)
  : Extract model fitted values
- [`fixef(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/fixef.md)
  : Extract fixed effects from galamm objects
- [`formula(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/formula.galamm.md)
  : Extract formula from fitted galamm object
- [`logLik(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/logLik.galamm.md)
  : Extract Log-Likelihood of galamm Object
- [`model.frame(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/model.frame.galamm.md)
  : Extract the model frame from a galamm object
- [`nobs(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/nobs.galamm.md)
  : Extract the Number of Observations from a galamm Fit
- [`predict(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/predict.galamm.md)
  : Predictions from a model at new data values
- [`print(`*`<VarCorr.galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/print.VarCorr.galamm.md)
  : Print method for variance-covariance objects
- [`ranef(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/ranef.galamm.md)
  : Extract random effects from galamm object.
- [`residuals(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/residuals.galamm.md)
  : Residuals of galamm objects
- [`response()`](https://lcbc-uio.github.io/galamm/reference/response.md)
  : Extract response values
- [`sigma(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/sigma.galamm.md)
  : Extract square root of dispersion parameter from galamm object
- [`vcov(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/vcov.galamm.md)
  : Calculate variance-covariance matrix for GALAMM fit

## Datasets

Simulated and real example datasets.

- [`cognition`](https://lcbc-uio.github.io/galamm/reference/cognition.md)
  : Simulated Data with Measurements of Cognitive Abilities
- [`diet`](https://lcbc-uio.github.io/galamm/reference/diet.md) : Diet
  Data
- [`epilep`](https://lcbc-uio.github.io/galamm/reference/epilep.md) :
  Epilepsy Data
- [`hsced`](https://lcbc-uio.github.io/galamm/reference/hsced.md) :
  Example Data with Heteroscedastic Residuals
- [`latent_covariates`](https://lcbc-uio.github.io/galamm/reference/latent_covariates.md)
  : Simulated Data with Latent and Observed Covariates Interaction
- [`latent_covariates_long`](https://lcbc-uio.github.io/galamm/reference/latent_covariates_long.md)
  : Simulated Longitudinal Data with Latent and Observed Covariates
  Interaction
- [`lifespan`](https://lcbc-uio.github.io/galamm/reference/lifespan.md)
  : Simulated Dataset with Lifespan Trajectories of Three Cognitive
  Domains
- [`mresp`](https://lcbc-uio.github.io/galamm/reference/mresp.md) :
  Simulated Mixed Response Data
- [`mresp_hsced`](https://lcbc-uio.github.io/galamm/reference/mresp_hsced.md)
  : Simulated Mixed Response Data with Heteroscedastic Residuals

## Optimization

Functions to aid in optimization.

- [`extract_optim_parameters(`*`<galamm>`*`)`](https://lcbc-uio.github.io/galamm/reference/extract_optim_parameters.galamm.md)
  : Extract parameters from fitted model for use as initial values
- [`galamm_control()`](https://lcbc-uio.github.io/galamm/reference/galamm_control.md)
  : Control values for galamm fit
