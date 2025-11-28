# Print method for summary GALAMM fits

Print method for summary GALAMM fits

## Usage

``` r
# S3 method for class 'summary.galamm'
print(x, digits = max(3, getOption("digits") - 3), ...)
```

## Arguments

- x:

  An object of class `summary.galamm` returned from
  [`summary.galamm`](https://lcbc-uio.github.io/galamm/reference/summary.galamm.md).

- digits:

  Number of digits to present in outputs.

- ...:

  Further arguments passed on to other methods. Currently used by
  [`stats::printCoefmat`](https://rdrr.io/r/stats/printCoefmat.html) for
  printing approximate significance of smooth terms.

## Value

Summary printed to screen. Invisibly returns the argument `x`.

## References

Bates DM, Mächler M, Bolker B, Walker S (2015). “Fitting Linear
Mixed-Effects Models Using Lme4.” *Journal of Statistical Software*,
**67**(1), 1–48. ISSN 1548-7660,
[doi:10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01) .

## See also

[`summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/summary.galamm.md)
for the summary function and
[`print()`](https://rdrr.io/r/base/print.html) for the generic function.

Other summary functions:
[`anova.galamm()`](https://lcbc-uio.github.io/galamm/reference/anova.galamm.md),
[`plot_smooth.galamm()`](https://lcbc-uio.github.io/galamm/reference/plot_smooth.galamm.md),
[`print.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.galamm.md),
[`summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/summary.galamm.md)

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
