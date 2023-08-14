
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The galamm package for fitting generalized additive latent and mixed models

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project status: active development but no stable release
yet.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/LCBC-UiO/galamm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LCBC-UiO/galamm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/LCBC-UiO/galamm/branch/main/graph/badge.svg)](https://app.codecov.io/gh/LCBC-UiO/galamm?branch=main)
<!-- badges: end -->

galamm is an R package for estimating generalized additive latent and
mixed models (GALAMM). The model framework was introduced by Sørensen,
Fjell, and Walhovd (2023), which is available with open access from the
[publisher’s website](https://doi.org/10.1007/s11336-023-09910-z). It is
an extension of the GLLAMM framework for multilevel latent variable
modeling detailed in Rabe-Hesketh, Skrondal, and Pickles (2004), in
particular by efficiently handling crossed random effects and
semiparametric estimation.

The package allows maximum likelihood estimation of complex multilevel
models (aka mixed models). In particular, models with any combination of
the following features are supported, with links to the corresponding
vignettes:

- [Linear mixed models with factor
  structures](https://lcbc-uio.github.io/galamm/articles/lmm_factor.html).
- [Generalized linear mixed models with factor
  structures](https://lcbc-uio.github.io/galamm/articles/glmm_factor.html).
- [Linear mixed models with heteroscedastic
  residuals](https://lcbc-uio.github.io/galamm/articles/lmm_heteroscedastic.html).
- [Mixed models with mixed response
  types](https://lcbc-uio.github.io/galamm/articles/mixed_response.html).
- [Generalized additive mixed models with factor
  structures](https://lcbc-uio.github.io/galamm/articles/semiparametric.html).

The package uses an interface similar to
[PLmixed](https://cran.r-project.org/package=PLmixed) (Rockwood and Jeon
2019), which fits generalized linear mixed models with factor
structures. However, for the types of models supported by both PLmixed
and galamm, galamm is usually considerably faster. The syntax for
defining models is close to that of
[lme4](https://cran.r-project.org/package=lme4) (Bates et al. 2015). If
your model can be estimated with lme4, then you should definitely prefer
that package, as it’s very mature, and also likely to be faster. galamm
should however give results very similar to lme4.

The core computations in galamm are done using sparse matrix methods
supported by [RcppEigen](https://cran.r-project.org/package=RcppEigen)
(Bates and Eddelbuettel 2013) and automatic differentiation using the
C++ library [autodiff](https://autodiff.github.io/) (Leal 2018). The
[optimization
vignette](https://lcbc-uio.github.io/galamm/articles/optimization.html)
provides more details.

## Installation

You can install the development version of galamm from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("LCBC-UiO/galamm")
```

## Example

``` r
library(galamm)
```

The dataframe `mresp` contains simulated data with mixed response types.

``` r
head(mresp)
#>   id         x          y itemgroup
#> 1  1 0.8638214  0.2866329         a
#> 2  1 0.7676133  2.5647490         a
#> 3  1 0.8812059  1.0000000         b
#> 4  1 0.2239725  1.0000000         b
#> 5  2 0.7215696 -0.4721698         a
#> 6  2 0.6924851  1.1750286         a
```

Responses in rows with `itemgroup = "a"` are normally distributed while
those in rows with `itemgroup = "b"` are binomially distributed. For a
given subject, identified by the `id` variable, both responses are
associated with the same underlying latent variable. We hence need to
model this process jointly, and the model is set up as follows:

``` r
mixed_resp <- galamm(
  formula = y ~ x + (0 + loading | id),
  data = mresp,
  family = c(gaussian, binomial),
  family_mapping = ifelse(mresp$itemgroup == "a", 1L, 2L),
  load.var = "itemgroup",
  lambda = list(matrix(c(1, NA), ncol = 1)),
  factor = list("loading")
)
#> N = 4, M = 20 machine precision = 2.22045e-16
#> At X0, 0 variables are exactly at the bounds
#> At iterate     0  f=       4764.7  |proj g|=       300.51
#> At iterate    10  f =       4619.3  |proj g|=       0.01885
#> 
#> iterations 11
#> function evaluations 12
#> segments explored during Cauchy searches 11
#> BFGS updates skipped 0
#> active bounds at final generalized Cauchy point 0
#> norm of the final projected gradient 0.00181656
#> final function value 4619.34
#> 
#> F = 4619.34
#> final  value 4619.341613 
#> converged
```

The summary function gives some information about the model fit.

``` r
summary(mixed_resp)
#> Generalized additive latent and mixed model fit by maximum marginal likelihood.
#> Formula: y ~ x + (0 + loading | id)
#>    Data: mresp
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>   3643.1   3674.5  -4619.3   3633.1     3995 
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.5360 -0.7078  0.2156  0.6456  2.5978 
#> 
#> Lambda:
#>   loading      SE
#> 1   1.000       .
#> 2   1.095 0.09982
#> 
#> Random effects:
#>  Groups   Name    Variance Std.Dev.
#>  id       loading 1.05     1.025   
#>  Residual         1.12     1.058   
#> Number of obs: 4000, groups:  id, 1000
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)    0.041    0.05803  0.7065
#> x              0.971    0.08594 11.2994
```

## How to cite this package

``` r
citation("galamm")
#> To cite package 'galamm' in publications use:
#> 
#>   Sørensen Ø, Walhovd K, Fjell A (2023). "Longitudinal Modeling of
#>   Age-Dependent Latent Traits with Generalized Additive Latent and
#>   Mixed Models." _Psychometrika_, *88*(2), 456-486.
#>   doi:10.1007/s11336-023-09910-z
#>   <https://doi.org/10.1007/s11336-023-09910-z>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Longitudinal Modeling of Age-Dependent Latent Traits with Generalized Additive Latent and Mixed Models},
#>     author = {{\O}ystein S{\o}rensen and Kristine B. Walhovd and Anders M. Fjell},
#>     journal = {Psychometrika},
#>     year = {2023},
#>     volume = {88},
#>     number = {2},
#>     pages = {456-486},
#>     doi = {10.1007/s11336-023-09910-z},
#>   }
```

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-batesFastElegantNumerical2013" class="csl-entry">

Bates, Douglas M, and Dirk Eddelbuettel. 2013. “Fast and Elegant
Numerical Linear Algebra Using the RcppEigen Package.” *Journal of
Statistical Software* 52 (February): 1–24.
<https://doi.org/10.18637/jss.v052.i05>.

</div>

<div id="ref-batesFittingLinearMixedEffects2015" class="csl-entry">

Bates, Douglas M, Martin Mächler, Ben Bolker, and Steve Walker. 2015.
“Fitting Linear Mixed-Effects Models Using Lme4.” *Journal of
Statistical Software* 67 (1): 1–48.
<https://doi.org/10.18637/jss.v067.i01>.

</div>

<div id="ref-lealAutodiffModernFast2018" class="csl-entry">

Leal, Allan M. M. 2018. “Autodiff, a Modern, Fast and Expressive C++
Library for Automatic Differentiation.”

</div>

<div id="ref-rabe-heskethGeneralizedMultilevelStructural2004"
class="csl-entry">

Rabe-Hesketh, Sophia, Anders Skrondal, and Andrew Pickles. 2004.
“Generalized Multilevel Structural Equation Modeling.” *Psychometrika*
69 (2): 167–90. <https://doi.org/10.1007/BF02295939>.

</div>

<div id="ref-rockwoodEstimatingComplexMeasurement2019"
class="csl-entry">

Rockwood, Nicholas J., and Minjeong Jeon. 2019. “Estimating Complex
Measurement and Growth Models Using the R Package PLmixed.”
*Multivariate Behavioral Research* 54 (2): 288–306.
<https://doi.org/10.1080/00273171.2018.1516541>.

</div>

<div id="ref-sorensenLongitudinalModelingAgeDependent2023"
class="csl-entry">

Sørensen, Øystein, Anders M. Fjell, and Kristine B. Walhovd. 2023.
“Longitudinal Modeling of Age-Dependent Latent Traits with Generalized
Additive Latent and Mixed Models.” *Psychometrika* 88 (2): 456–86.
<https://doi.org/10.1007/s11336-023-09910-z>.

</div>

</div>
