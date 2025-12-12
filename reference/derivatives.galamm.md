# Derivatives of estimated smooth via finite differences

This function uses
[`gratia::derivatives`](https://gavinsimpson.github.io/gratia/reference/derivatives.html)
to compute derivatives of estimated smooth via finite differences. See
[`gratia::derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.html)
for details. When `object` is not of class `galamm`, it is forwarded to
[`gratia::derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.html).

## Usage

``` r
# S3 method for class 'galamm'
derivatives(object, ...)
```

## Arguments

- object:

  An object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- ...:

  Other arguments passed on to
  [`gratia::derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.html).

## Value

A tibble.

## See also

Other details of model fit:
[`VarCorr()`](https://lcbc-uio.github.io/galamm/reference/VarCorr.md),
[`appraise.galamm()`](https://lcbc-uio.github.io/galamm/reference/appraise.galamm.md),
[`coef.galamm()`](https://lcbc-uio.github.io/galamm/reference/coef.galamm.md),
[`confint.galamm()`](https://lcbc-uio.github.io/galamm/reference/confint.galamm.md),
[`deviance.galamm()`](https://lcbc-uio.github.io/galamm/reference/deviance.galamm.md),
[`factor_loadings.galamm()`](https://lcbc-uio.github.io/galamm/reference/factor_loadings.galamm.md),
[`family.galamm()`](https://lcbc-uio.github.io/galamm/reference/family.galamm.md),
[`fitted.galamm()`](https://lcbc-uio.github.io/galamm/reference/fitted.galamm.md),
[`fixef()`](https://lcbc-uio.github.io/galamm/reference/fixef.md),
[`formula.galamm()`](https://lcbc-uio.github.io/galamm/reference/formula.galamm.md),
[`llikAIC()`](https://lcbc-uio.github.io/galamm/reference/llikAIC.md),
[`logLik.galamm()`](https://lcbc-uio.github.io/galamm/reference/logLik.galamm.md),
[`model.frame.galamm()`](https://lcbc-uio.github.io/galamm/reference/model.frame.galamm.md),
[`nobs.galamm()`](https://lcbc-uio.github.io/galamm/reference/nobs.galamm.md),
[`predict.galamm()`](https://lcbc-uio.github.io/galamm/reference/predict.galamm.md),
[`print.VarCorr.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.VarCorr.galamm.md),
[`ranef.galamm()`](https://lcbc-uio.github.io/galamm/reference/ranef.galamm.md),
[`residuals.galamm()`](https://lcbc-uio.github.io/galamm/reference/residuals.galamm.md),
[`response()`](https://lcbc-uio.github.io/galamm/reference/response.md),
[`sigma.galamm()`](https://lcbc-uio.github.io/galamm/reference/sigma.galamm.md),
[`vcov.galamm()`](https://lcbc-uio.github.io/galamm/reference/vcov.galamm.md)

## Examples

``` r
dat <- subset(cognition, domain == 1 & item == "11")
dat$y <- dat$y[, 1]
mod <- galamm(y ~ s(x) + (1 | id), data = dat)

dd <- derivatives(mod)
draw(dd)

```
