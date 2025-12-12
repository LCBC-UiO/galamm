# Compare likelihoods of galamm objects

Anova function for comparing different GALAMMs fitted on the same data.

## Usage

``` r
# S3 method for class 'galamm'
anova(object, ...)
```

## Arguments

- object:

  An object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- ...:

  Other fitted models of class `galamm`. Currently, if no models are
  provided in this argument, no table will be returned.

## Value

A table with model comparison metric.

## References

Bates DM, Mächler M, Bolker B, Walker S (2015). “Fitting Linear
Mixed-Effects Models Using Lme4.” *Journal of Statistical Software*,
**67**(1), 1–48. ISSN 1548-7660,
[doi:10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01) .

## See also

[`summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/summary.galamm.md)
for the summary method and
[`anova()`](https://rdrr.io/r/stats/anova.html) for the generic
function.

Other summary functions:
[`draw.galamm()`](https://lcbc-uio.github.io/galamm/reference/draw.galamm.md),
[`plot_smooth.galamm()`](https://lcbc-uio.github.io/galamm/reference/plot_smooth.galamm.md),
[`print.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.galamm.md),
[`print.summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.summary.galamm.md),
[`summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/summary.galamm.md)

## Author

Some of the source code for this function is adapted from
`lme4:::anova.merMod`, with authors Douglas M. Bates, Martin Maechler,
Ben Bolker, and Steve Walker.

## Examples

``` r
# Poisson GLMM
count_mod <- galamm(
  formula = y ~ lbas * treat + lage + v4 + (1 | subj),
  data = epilep, family = poisson
)

# Model without interaction
count_mod0 <- galamm(
  formula = y ~ lbas + treat + lage + v4 + (1 | subj),
  data = epilep, family = poisson
)

# Model comparison
anova(count_mod, count_mod0)
#> Data: epilep
#> Models:
#> count_mod0: y ~ lbas + treat + lage + v4 + (1 | subj)
#> count_mod: y ~ lbas * treat + lage + v4 + (1 | subj)
#>            npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
#> count_mod0    6 1345.4 1366.2 -666.72   407.01                       
#> count_mod     7 1344.7 1369.0 -665.36   407.01 2.7208  1    0.09905 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
