# Print method for GALAMM fits

Print method for GALAMM fits

## Usage

``` r
# S3 method for class 'galamm'
print(x, ...)
```

## Arguments

- x:

  An object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- ...:

  Further arguments passed on to other methods. Currently not used.

## Value

Summary printed to screen. Invisibly returns the argument `x`.

## See also

[`summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/summary.galamm.md)
for the summary function and
[`print()`](https://rdrr.io/r/base/print.html) for the generic.

Other summary functions:
[`anova.galamm()`](https://lcbc-uio.github.io/galamm/reference/anova.galamm.md),
[`draw.galamm()`](https://lcbc-uio.github.io/galamm/reference/draw.galamm.md),
[`plot_smooth.galamm()`](https://lcbc-uio.github.io/galamm/reference/plot_smooth.galamm.md),
[`print.summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.summary.galamm.md),
[`summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/summary.galamm.md)

## Examples

``` r
# Linear mixed model with heteroscedastic residuals
mod <- galamm(
  formula = y ~ x + (1 | id),
  dispformula = ~ (1 | item),
  data = hsced
)

print(mod)
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
