# Extract formula from fitted galamm object

Extract formula from fitted galamm object

## Usage

``` r
# S3 method for class 'galamm'
formula(x, ...)
```

## Arguments

- x:

  Object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- ...:

  Optional arguments passed on to other methods. Currently not used.

## Value

The formula used to fit the model.

## See also

Other details of model fit:
[`VarCorr()`](https://lcbc-uio.github.io/galamm/reference/VarCorr.md),
[`appraise.galamm()`](https://lcbc-uio.github.io/galamm/reference/appraise.galamm.md),
[`coef.galamm()`](https://lcbc-uio.github.io/galamm/reference/coef.galamm.md),
[`confint.galamm()`](https://lcbc-uio.github.io/galamm/reference/confint.galamm.md),
[`derivatives.galamm()`](https://lcbc-uio.github.io/galamm/reference/derivatives.galamm.md),
[`deviance.galamm()`](https://lcbc-uio.github.io/galamm/reference/deviance.galamm.md),
[`factor_loadings.galamm()`](https://lcbc-uio.github.io/galamm/reference/factor_loadings.galamm.md),
[`family.galamm()`](https://lcbc-uio.github.io/galamm/reference/family.galamm.md),
[`fitted.galamm()`](https://lcbc-uio.github.io/galamm/reference/fitted.galamm.md),
[`fixef()`](https://lcbc-uio.github.io/galamm/reference/fixef.md),
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
# Mixed response model ------------------------------------------------------

# The mresp dataset contains a mix of binomial and Gaussian responses.

# We need to estimate a factor loading which scales the two response types.
loading_matrix <- matrix(c(1, NA), ncol = 1)

# Define mapping to families.
families <- gfam(list(gaussian, binomial))


# Fit the model
mod <- galamm(
  formula = y ~ x + (0 + level | id),
  data = mresp,
  family = families,
  factor = "level",
  load_var = "itemgroup",
  lambda = loading_matrix
)

# Formula
formula(mod)
#> y ~ x + (0 + level | id)
```
