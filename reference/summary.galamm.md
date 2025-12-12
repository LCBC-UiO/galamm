# Summarizing GALAMM fits

Summary method for class "galamm".

## Usage

``` r
# S3 method for class 'galamm'
summary(object, ...)
```

## Arguments

- object:

  An object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- ...:

  Further arguments passed on to other methods. Currently not used.

## Value

A list of summary statistics of the fitted model of class
`summary.galamm`, containing the following elements:

- `AICtab` a table of model fit measures, returned by
  [`llikAIC`](https://lcbc-uio.github.io/galamm/reference/llikAIC.md).

- `call` the matched call used when fitting the model.

- `fixef` a matrix with fixed effect estimated, returned by
  [`fixef`](https://lcbc-uio.github.io/galamm/reference/fixef.md).

- `gam` List containing information about smooth terms in the model. If
  no smooth terms are contained in the model, then it is a list of
  length zero.

- `model` a list with various elements related to the model setup and
  fit. See
  [`?galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md) for
  details.

- `parameters` A list object with model parameters and related
  information. See
  [`?galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md) for
  details.

- `Lambda` An object containing the estimated factor loadings. Returned
  from
  [`factor_loadings.galamm`](https://lcbc-uio.github.io/galamm/reference/factor_loadings.galamm.md).
  If there are no estimated factor loadings, then this object is `NULL`.

- `random_effects` a list containing the random effects. See
  [`?galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md) for
  details.

- `VarCorr` An object of class `VarCorr.galamm`, returned from
  [`VarCorr.galamm`](https://lcbc-uio.github.io/galamm/reference/VarCorr.md).

- `weights` An object containing information about estimated variance
  functions, when there are heteroscedastic residuals. Otherwise the
  object is `NULL`.

## See also

[`print.summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.summary.galamm.md)
for the print method and
[`summary()`](https://rdrr.io/r/base/summary.html) for the generic.

Other summary functions:
[`anova.galamm()`](https://lcbc-uio.github.io/galamm/reference/anova.galamm.md),
[`draw.galamm()`](https://lcbc-uio.github.io/galamm/reference/draw.galamm.md),
[`plot_smooth.galamm()`](https://lcbc-uio.github.io/galamm/reference/plot_smooth.galamm.md),
[`print.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.galamm.md),
[`print.summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.summary.galamm.md)

## Author

Some of the code for producing summary information has been derived from
the summary methods of `mgcv` (author: Simon Wood) and `lme4` (Bates et
al. 2015) (authors: Douglas M. Bates, Martin Maechler, Ben Bolker, and
Steve Walker).

## Examples

``` r
# Linear mixed model with heteroscedastic residuals
mod <- galamm(
  formula = y ~ x + (1 | id),
  dispformula = ~ (1 | item),
  data = hsced
)

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
