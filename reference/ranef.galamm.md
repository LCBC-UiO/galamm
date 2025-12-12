# Extract random effects from galamm object.

Extract random effects from galamm object.

## Usage

``` r
# S3 method for class 'galamm'
ranef(object, ...)
```

## Arguments

- object:

  An object of class `galamm`, returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- ...:

  Optional parameters passed on to other methods. Currently not used.

## Value

An object of class `ranef.galamm`, containing the requested random
effects.

## References

Bates DM, Mächler M, Bolker B, Walker S (2015). “Fitting Linear
Mixed-Effects Models Using Lme4.” *Journal of Statistical Software*,
**67**(1), 1–48. ISSN 1548-7660,
[doi:10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01) .

## See also

[`fixef.galamm()`](https://lcbc-uio.github.io/galamm/reference/fixef.md)
for fixed effects and
[`coef.galamm()`](https://lcbc-uio.github.io/galamm/reference/coef.galamm.md)
for coefficients more generally.

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
[`formula.galamm()`](https://lcbc-uio.github.io/galamm/reference/formula.galamm.md),
[`llikAIC()`](https://lcbc-uio.github.io/galamm/reference/llikAIC.md),
[`logLik.galamm()`](https://lcbc-uio.github.io/galamm/reference/logLik.galamm.md),
[`model.frame.galamm()`](https://lcbc-uio.github.io/galamm/reference/model.frame.galamm.md),
[`nobs.galamm()`](https://lcbc-uio.github.io/galamm/reference/nobs.galamm.md),
[`predict.galamm()`](https://lcbc-uio.github.io/galamm/reference/predict.galamm.md),
[`print.VarCorr.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.VarCorr.galamm.md),
[`residuals.galamm()`](https://lcbc-uio.github.io/galamm/reference/residuals.galamm.md),
[`response()`](https://lcbc-uio.github.io/galamm/reference/response.md),
[`sigma.galamm()`](https://lcbc-uio.github.io/galamm/reference/sigma.galamm.md),
[`vcov.galamm()`](https://lcbc-uio.github.io/galamm/reference/vcov.galamm.md)

## Author

This function is derived from
[`lme4::ranef.merMod`](https://rdrr.io/pkg/lme4/man/ranef.html), written
by Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker.

## Examples

``` r
# Poisson GLMM
count_mod <- galamm(
  formula = y ~ lbas * treat + lage + v4 + (1 | subj),
  data = epilep, family = poisson
)

# Extract random effects
ranef(count_mod)
#> $subj
#>     (Intercept)
#> 1   0.054801194
#> 2   0.067128734
#> 3   0.339005443
#> 4   0.156539399
#> 5   0.018769146
#> 6  -0.203361347
#> 7  -0.121249992
#> 8   0.445388051
#> 9  -0.183126098
#> 10  0.941223293
#> 11  0.164707632
#> 12 -0.013318772
#> 13 -0.069755556
#> 14 -0.080784157
#> 15 -0.220238514
#> 16 -0.856980819
#> 17 -0.756729929
#> 18  0.192712910
#> 19 -0.247883354
#> 20 -0.109262155
#> 21  0.020112309
#> 22  0.336336442
#> 23 -0.312726250
#> 24  0.097464659
#> 25  0.959569912
#> 26 -0.457892584
#> 27  0.039653228
#> 28  0.263425805
#> 29 -0.311198889
#> 30 -0.155081893
#> 31 -0.320528837
#> 32  0.539457657
#> 33  0.460522482
#> 34 -0.305186294
#> 35  1.018964462
#> 36  0.548834571
#> 37  0.275759175
#> 38 -0.688740932
#> 39 -0.046734600
#> 40  0.005348547
#> 41 -0.569016659
#> 42  0.134020686
#> 43  0.392476777
#> 44  0.016707046
#> 45  0.100750799
#> 46  0.381237132
#> 47  0.109689790
#> 48 -0.373107851
#> 49  0.686635579
#> 50 -0.212730301
#> 51 -0.194902788
#> 52 -0.772084418
#> 53  0.443822432
#> 54 -0.389679998
#> 55  0.201819631
#> 56  1.101943180
#> 57 -0.645241909
#> 58 -0.938185103
#> 59  0.094489081
#> 
#> attr(,"class")
#> [1] "ranef.galamm"
```
