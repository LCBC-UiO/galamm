# Extract variance and correlation components from model

Extract variance and correlation components from model

## Usage

``` r
# S3 method for class 'galamm'
VarCorr(x, sigma = 1, ...)
```

## Arguments

- x:

  An object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- sigma:

  Numeric value used to multiply the standard deviations. Defaults to 1.

- ...:

  Other arguments passed onto other methods. Currently not used.

## Value

An object of class `c("VarCorr.galamm", "VarCorr.merMod")`.

## See also

[`print.VarCorr.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.VarCorr.galamm.md)
for the print function.

Other details of model fit:
[`appraise.galamm()`](https://lcbc-uio.github.io/galamm/reference/appraise.galamm.md),
[`coef.galamm()`](https://lcbc-uio.github.io/galamm/reference/coef.galamm.md),
[`confint.galamm()`](https://lcbc-uio.github.io/galamm/reference/confint.galamm.md),
[`derivatives.galamm()`](https://lcbc-uio.github.io/galamm/reference/derivatives.galamm.md),
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
# Linear mixed model with heteroscedastic residuals
mod <- galamm(
  formula = y ~ x + (1 | id),
  dispformula = ~ (1 | item),
  data = hsced
)

# Extract information on variance and covariance
VarCorr(mod)
#>  Groups   Name        Std.Dev. Variance
#>  id       (Intercept) 0.99400  0.98804 
#>  Residual             0.97964  0.95970 

# Convert to data frame
# (this invokes lme4's function as.data.frame.VarCorr.merMod)
as.data.frame(VarCorr(mod))
#>        grp        var1 var2      vcov     sdcor
#> 1       id (Intercept) <NA> 0.9880425 0.9940033
#> 2 Residual        <NA> <NA> 0.9596999 0.9796427
```
