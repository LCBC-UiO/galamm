# Extract factor loadings from galamm object

Extract factor loadings from galamm object

## Usage

``` r
# S3 method for class 'galamm'
factor_loadings(object)
```

## Arguments

- object:

  Object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

## Value

A matrix containing the estimated factor loadings with corresponding
standard deviations.

## Details

This function has been named `factor_loadings` rather than just
`loadings` to avoid conflict with
[`stats::loadings`](https://rdrr.io/r/stats/loadings.html).

## See also

[`fixef.galamm()`](https://lcbc-uio.github.io/galamm/reference/fixef.md)
for fixed regression coefficients,
[`confint.galamm()`](https://lcbc-uio.github.io/galamm/reference/confint.galamm.md)
for confidence intervals, and
[`coef.galamm()`](https://lcbc-uio.github.io/galamm/reference/coef.galamm.md)
for coefficients more generally.

Other details of model fit:
[`VarCorr()`](https://lcbc-uio.github.io/galamm/reference/VarCorr.md),
[`coef.galamm()`](https://lcbc-uio.github.io/galamm/reference/coef.galamm.md),
[`confint.galamm()`](https://lcbc-uio.github.io/galamm/reference/confint.galamm.md),
[`deviance.galamm()`](https://lcbc-uio.github.io/galamm/reference/deviance.galamm.md),
[`family.galamm()`](https://lcbc-uio.github.io/galamm/reference/family.galamm.md),
[`fitted.galamm()`](https://lcbc-uio.github.io/galamm/reference/fitted.galamm.md),
[`fixef()`](https://lcbc-uio.github.io/galamm/reference/fixef.md),
[`formula.galamm()`](https://lcbc-uio.github.io/galamm/reference/formula.galamm.md),
[`llikAIC()`](https://lcbc-uio.github.io/galamm/reference/llikAIC.md),
[`logLik.galamm()`](https://lcbc-uio.github.io/galamm/reference/logLik.galamm.md),
[`nobs.galamm()`](https://lcbc-uio.github.io/galamm/reference/nobs.galamm.md),
[`predict.galamm()`](https://lcbc-uio.github.io/galamm/reference/predict.galamm.md),
[`print.VarCorr.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.VarCorr.galamm.md),
[`ranef.galamm()`](https://lcbc-uio.github.io/galamm/reference/ranef.galamm.md),
[`residuals.galamm()`](https://lcbc-uio.github.io/galamm/reference/residuals.galamm.md),
[`response()`](https://lcbc-uio.github.io/galamm/reference/response.md),
[`sigma.galamm()`](https://lcbc-uio.github.io/galamm/reference/sigma.galamm.md),
[`vcov.galamm()`](https://lcbc-uio.github.io/galamm/reference/vcov.galamm.md)

## Author

The example for this function comes from `PLmixed`, with authors
Nicholas Rockwood and Minjeong Jeon (Rockwood and Jeon 2019) .

## Examples

``` r
# Logistic mixed model with factor loadings, example from PLmixed
data("IRTsim", package = "PLmixed")

# Reduce data size for the example to run faster
IRTsub <- IRTsim[IRTsim$item < 4, ]
IRTsub <- IRTsub[sample(nrow(IRTsub), 300), ]
IRTsub$item <- factor(IRTsub$item)

# Fix loading for first item to 1, and estimate the two others freely
loading_matrix <- matrix(c(1, NA, NA), ncol = 1)

# Estimate model
mod <- galamm(y ~ item + (0 + ability | sid) + (0 + ability | school),
  data = IRTsub, family = binomial, load_var = "item",
  factor = "ability", lambda = loading_matrix
)

# Show estimated factor loadings, with standard errors
factor_loadings(mod)
#>           ability        SE
#> lambda1 1.0000000        NA
#> lambda2 0.3570000 0.2113691
#> lambda3 0.3713262 0.2206723
```
