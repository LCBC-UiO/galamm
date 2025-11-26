# Confidence intervals for model parameters

Confidence intervals for model parameters

## Usage

``` r
# S3 method for class 'galamm'
confint(object, parm, level = 0.95, method = "Wald", ...)
```

## Arguments

- object:

  An object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- parm:

  Parameters for which to compute intervals. Use `"theta"` to get all
  variance parameters, `"beta"` to get all fixed regression
  coefficients, `"lambda"` to get all factor loadings, and `"weights"`
  to get all weights. The parameter can also be given as a numeric
  vector with indices specifying the parameters. When given as
  characters, the arguments are case sensitive.

- level:

  Decimal number specifying the confidence level. Defaults to 0.95.

- method:

  Character of length one specifying the type of confidence interval.
  Currently only "Wald" is available. The argument is case sensitive.

- ...:

  Other arguments passed on to other methods. Currently not used.

## Value

A matrix with the requested confidence intervals.

## See also

[`fixef.galamm()`](https://lcbc-uio.github.io/galamm/reference/fixef.md)
for fixed effects,
[`coef.galamm()`](https://lcbc-uio.github.io/galamm/reference/coef.galamm.md)
for coefficients more generally, and
[`vcov.galamm()`](https://lcbc-uio.github.io/galamm/reference/vcov.galamm.md)
for the variance-covariance matrix.
[`confint()`](https://rdrr.io/r/stats/confint.html) is the generic
function.

Other details of model fit:
[`VarCorr()`](https://lcbc-uio.github.io/galamm/reference/VarCorr.md),
[`coef.galamm()`](https://lcbc-uio.github.io/galamm/reference/coef.galamm.md),
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
# Poisson GLMM
count_mod <- galamm(
  formula = y ~ lbas * treat + lage + v4 + (1 | subj),
  data = epilep, family = poisson
)

confint(count_mod, parm = "beta", level = .99)
#>                  0.5 %      99.5 %
#> (Intercept)  1.5239392  2.06319914
#> lbas         0.5471847  1.22182317
#> treat       -0.7155511  0.04562592
#> lage        -0.4081356  1.37730585
#> v4          -0.3016653 -0.02050959
#> lbas:treat  -0.1843249  0.86110474
```
