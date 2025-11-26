# Extract square root of dispersion parameter from galamm object

Extracts the square root of the dispersion parameter(s) from an object
of class `galamm`, returned from
[`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md). In
the case of conditionally Gaussian responses, this is the residual
standard deviation. When there are multiple dispersion parameters, e.g.,
with mixed response type models, the square root of all of them are
returned in a numeric vector.

## Usage

``` r
# S3 method for class 'galamm'
sigma(object, ...)
```

## Arguments

- object:

  An object of class `galamm`, returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- ...:

  Optional parameters passed on to other methods. Currently not used.

## Value

The square root of one or more dispersion parameters.

## See also

[`galamm()`](https://lcbc-uio.github.io/galamm/reference/galamm.md)

Other details of model fit:
[`VarCorr()`](https://lcbc-uio.github.io/galamm/reference/VarCorr.md),
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
[`vcov.galamm()`](https://lcbc-uio.github.io/galamm/reference/vcov.galamm.md)

## Examples

``` r
# Linear mixed model with heteroscedastic residuals
mod <- galamm(
  formula = y ~ x + (1 | id),
  dispformula = ~ (1 | item),
  data = hsced
)

# Extract residual standard deviation.
sigma(mod)
#> [1] 0.9796427

# The residual standard deviation applies to the base case. The variance
# function shown in the model output shows the estimated multiplier for
# various grouping levels:
summary(mod)
#> GALAMM fit by maximum marginal likelihood.
#> Formula: y ~ x + (1 | id)
#>    Data: hsced
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>   4126.3   4151.7  -2058.1   4116.3     1195 
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -5.6545 -0.7105  0.0286  0.6827  4.3261 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  id       (Intercept) 0.9880   0.9940  
#>  Residual             0.9597   0.9796  
#> Number of obs: 1200, groups:  id, 200
#> 
#> Variance function:
#>     1     2 
#> 1.000 1.995 
#> 
#> Fixed effects:
#>             Estimate Std. Error t value  Pr(>|t|)
#> (Intercept)   0.1289     0.0992   1.299 1.938e-01
#> x             0.7062     0.1213   5.822 5.819e-09
#> 
#> 
```
