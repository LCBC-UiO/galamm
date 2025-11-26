# Residuals of galamm objects

Computes residuals for models fit with
[`galamm()`](https://lcbc-uio.github.io/galamm/reference/galamm.md)
using the definitions in Chapter 8 of Dunn and Smyth (2018) . Define
\\y\\ as the response and \\\hat{\mu}\\ as the model fit. Importantly,
\\\hat{\mu}\\ includes all random effects. Also define \\V(\cdot)\\ as
the variance function of the model family, and \\w\\ as the weight. The
Pearson residual is then \$\$r\_{P} = (y - \hat{\mu})/\sqrt{V(\hat{\mu})
/ w}.\$\$ Furthermore, let \\sgn(\cdot)\\ be the function which returns
the sign of its argument and let \\d(y, \hat{\mu})\\ be the model
deviance. The deviance residual is then \$\$r\_{D} = sgn(y - \hat{\mu})
\sqrt{w d(y, \hat{\mu})}.\$\$

## Usage

``` r
# S3 method for class 'galamm'
residuals(object, type = c("pearson", "deviance"), ...)
```

## Arguments

- object:

  An object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- type:

  Character of length one describing the type of residuals to be
  returned. One of `"pearson"` and `"deviance"`. Argument is case
  sensitive.

- ...:

  Optional arguments passed on to other methods. Currently not used.

## Value

Numeric vector of residual values.

## References

Dunn PK, Smyth GK (2018). *Generalized Linear Models With Examples in
R*, Springer Texts in Statistics. Springer, New York, NY. ISBN
978-1-4419-0117-0 978-1-4419-0118-7,
[doi:10.1007/978-1-4419-0118-7](https://doi.org/10.1007/978-1-4419-0118-7)
.

## See also

[`fitted.galamm()`](https://lcbc-uio.github.io/galamm/reference/fitted.galamm.md)
for model fitted values,
[`predict.galamm()`](https://lcbc-uio.github.io/galamm/reference/predict.galamm.md)
for model predictions, and
[`plot.galamm()`](https://lcbc-uio.github.io/galamm/reference/plot.galamm.md)
for diagnostic plots. The generic function is
[`residuals()`](https://rdrr.io/r/stats/residuals.html).

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

# Extract residuals
residuals(count_mod)
#>   [1]  0.75134825 -0.30588389 -0.30588389 -0.02647642 -0.29969105  0.75942563
#>   [7] -0.29969105 -0.02030696 -0.31982773  0.94348448 -1.58313993  1.96255720
#>  [13]  0.43844044  0.43844044 -1.23495860  0.76431835 -1.92290673  0.98985619
#>  [19] -1.39331347  2.54290384 -0.37901975 -1.61176436  0.85372485  0.87239664
#>  [25]  1.53250415  0.42157251 -1.80029078 -0.45686210  3.20970074 -0.85522137
#>  [31] -0.24548305 -1.89587259 -0.37109232  0.04054904  0.04054904 -0.01046534
#>  [37]  2.32295391  1.96014958 -0.57948072 -2.54300871  2.19282080 -1.20593505
#>  [43] -2.66254471  1.98848496  1.50225502 -0.64560897  0.07034569 -1.02528638
#>  [49] -0.11018739 -0.11018739  0.86264510 -0.84232934 -1.20333646 -0.60001269
#>  [55]  0.30497297  1.51905875  0.11484915  2.14376752 -1.40683963 -1.16387436
#>  [61]  2.65545389 -2.24478741 -2.24478741  0.34313328 -1.52938364 -1.52938364
#>  [67]  0.43219090  0.71507238  0.93383050 -0.48618501 -0.66368695  0.38154208
#>  [73] -0.56482022  0.41685518 -1.05565792  0.78036317 -0.61348596 -2.06574589
#>  [79]  0.83877398  1.76694279 -0.32319792  0.20279343 -0.32319792  0.52638912
#>  [85] -0.15886014  0.39262021 -0.15886014  0.71796768 -0.88312263 -0.36313065
#>  [91] -0.36313065  1.04374857  0.11210337  1.55462045 -2.05167224  0.56867360
#>  [97] -3.01755708 -2.01950534  6.63027648 -1.03914103 -0.02145063 -0.72321509
#> [103] -0.02145063 -0.55408107  0.27795300 -0.97310964  0.90348433 -0.11893239
#> [109] -0.13340625  0.41112684 -0.13340625  0.15259422 -0.06770284  0.82764610
#> [115] -0.66460214 -0.50349772  0.20892340 -0.15792875  0.57577555 -0.92446003
#> [121] -1.46575702  1.26320826  0.58096694 -1.35232878  0.10795686  1.89482700
#> [127] -1.08328990  0.38775621 -1.16421688  0.73022624  1.20383702  0.10529799
#> [133]  0.46538682 -0.09615929 -1.21925153  0.18294961  0.90033033 -0.27288190
#> [139]  0.19640299  0.13717193  0.17355958 -0.29134663  1.10337200  0.03108554
#> [145] -0.20516397  1.11018319 -1.52051113  1.44850131 -1.49633342  0.02175214
#> [151]  0.02175214  0.44848458 -1.29984938  3.78574203 -2.02636244 -0.57121746
#> [157]  0.95126708 -0.03257705 -0.03257705 -0.93776498 -1.46467211 -0.09917878
#> [163]  1.26631454 -1.35132783  1.17204252  0.59252146 -1.72556281  0.29235793
#> [169] -1.34839757 -0.60923644  2.10102101  0.26122160  1.25864008 -0.66816476
#> [175] -1.43888670  0.94731198  2.83566561 -0.95477231 -1.27064214 -0.52432300
#> [181] -0.35539104 -0.35539104  0.48257860  1.62375415 -0.76701612  0.63019867
#> [187] -0.06840872  0.38751946  0.77476967 -0.15207809 -1.07892584 -0.99543270
#> [193]  2.75568498 -1.44160278 -0.64752131 -0.38687286  0.21353124 -0.31387209
#> [199] -0.84127542  0.53720677  0.39735678 -0.36116194 -0.74042129  0.44481689
#> [205] -1.29929849 -0.21360713 -1.29929849  1.24229858  0.06700104 -1.59599420
#> [211]  2.44270852 -0.53607134  1.14333078 -0.40015859  0.11433786 -1.79323794
#> [217] -0.35757659  0.68406717  0.16324529 -0.07793957 -3.19183238  3.13136837
#> [223]  1.98169550 -0.71780112 -0.15016611  0.52039502 -1.49128836 -0.64907910
#> [229] -0.98405084 -0.98405084 -0.98405084 -0.90789964 -0.94826426  0.94946156
#> [235]  0.31688629 -0.08723898
```
