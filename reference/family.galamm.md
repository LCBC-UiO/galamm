# Extract family or families from fitted galamm

This function returns a list of families for an object of class
`galamm`, returned from
[`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

## Usage

``` r
# S3 method for class 'galamm'
family(object, ...)
```

## Arguments

- object:

  An object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- ...:

  Optional arguments passed on to other methods. Currently not used.

## Value

A list of family objects.

## See also

[`galamm()`](https://lcbc-uio.github.io/galamm/reference/galamm.md)

Other details of model fit:
[`VarCorr()`](https://lcbc-uio.github.io/galamm/reference/VarCorr.md),
[`coef.galamm()`](https://lcbc-uio.github.io/galamm/reference/coef.galamm.md),
[`confint.galamm()`](https://lcbc-uio.github.io/galamm/reference/confint.galamm.md),
[`deviance.galamm()`](https://lcbc-uio.github.io/galamm/reference/deviance.galamm.md),
[`factor_loadings.galamm()`](https://lcbc-uio.github.io/galamm/reference/factor_loadings.galamm.md),
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
# Mixed response model
loading_matrix <- matrix(c(1, NA), ncol = 1)
families <- gfam(list(gaussian, binomial))

mixed_resp <- galamm(
  formula = y ~ x + (0 + level | id),
  data = mresp,
  family = families,
  load_var = "itemgroup",
  lambda = loading_matrix,
  factor = "level"
)

# This model has two family objects
family(mixed_resp)
#> [[1]]
#> 
#> Family: gaussian 
#> Link function: identity 
#> 
#> 
#> [[2]]
#> 
#> Family: binomial 
#> Link function: logit 
#> 
#> 
#> attr(,"class")
#> [1] "galamm_extended_family"
```
