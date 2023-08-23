
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Generalized Additive Latent and Mixed Models

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project status: active development but no stable release
yet.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/LCBC-UiO/galamm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LCBC-UiO/galamm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/LCBC-UiO/galamm/branch/main/graph/badge.svg)](https://app.codecov.io/gh/LCBC-UiO/galamm?branch=main)
<!-- badges: end -->

galamm estimates generalized additive latent and mixed models (GALAMMs).
The model framework and the computational algorithms were introduced in
Sørensen, Fjell, and Walhovd (2023), which is [freely available from
Psychometrika](https://doi.org/10.1007/s11336-023-09910-z). It is an
extension of the GLLAMM framework for multilevel latent variable
modeling detailed in Rabe-Hesketh, Skrondal, and Pickles (2004) and
Skrondal and Rabe-Hesketh (2004), in particular by efficiently handling
crossed random effects and semiparametric estimation.

## What Can the Package Do?

Many applications, particularly in the social sciences, require modeling
capabilities beyond what is easily supported and computationally
feasible with popular R packages like
[mgcv](https://cran.r-project.org/package=mgcv) (Wood 2017),
[lavaan](https://lavaan.ugent.be/) (Rosseel 2012),
[lme4](https://cran.r-project.org/package=lme4) (Bates et al. 2015), and
[OpenMx](https://openmx.ssri.psu.edu/) (Neale et al. 2016), as well as
the Stata based [GLLAMM](http://www.gllamm.org/) software (Rabe-Hesketh,
Skrondal, and Pickles 2004, 2005). In particular, to maximally utilize
large datasets available today, it is typically necessary to combine
tools from latent variable modeling, hierarchical modeling, and
semiparametric estimation. While this is possible with Bayesian
hierarchical models and tools like [Stan](https://mc-stan.org/), it
requires considerable expertise and may be beyond scope for a single
data analysis project.

The goal of galamm is to enable estimation of models with any
combination of the following features (click the links to go to the
relevant vignette):

- Data with an arbitrary number of grouping levels, both crossed and
  hierarchical.
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

Random effects are defined using
[lme4](https://cran.r-project.org/package=lme4) syntax, and the syntax
for factor structures are close to that of
[PLmixed](https://cran.r-project.org/package=PLmixed) (Rockwood and Jeon
2019). However, for the types of models supported by both PLmixed and
galamm, galamm is usually considerably faster. Smooth terms, as in
generalized additive mixed models, use the same syntax as
[mgcv](https://cran.r-project.org/package=mgcv).

For most users, it should not be necessary to think about how the actual
computations are performed, although they are detailed in the
[optimization
vignette](https://lcbc-uio.github.io/galamm/articles/optimization.html).
In short, the core computations are done using sparse matrix methods
supported by [RcppEigen](https://cran.r-project.org/package=RcppEigen)
(Bates and Eddelbuettel 2013) and automatic differentiation using the
C++ library [autodiff](https://autodiff.github.io/) (Leal 2018).

## Where Do I Start?

To get started, take a look at the [introductory
vignette](https://lcbc-uio.github.io/galamm/articles/introduction.html).

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
#> Loading required package: mgcv
#> Loading required package: nlme
#> This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.
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
```

The summary function gives some information about the model fit.

``` r
summary(mixed_resp)
#> Generalized additive latent and mixed model fit by maximum marginal likelihood.
#> Formula: y ~ x + (0 + loading | id)
#>    Data: mresp
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>   9248.7   9280.2  -4619.3   3633.1     3995 
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.5360 -0.7078  0.2156  0.6456  2.5978 
#> 
#> Lambda:
#>         loading      SE
#> lambda1   1.000       .
#> lambda2   1.095 0.09982
#> 
#> Random effects:
#>  Groups Name    Variance Std.Dev.
#>  id     loading 1.05     1.025   
#> Number of obs: 4000, groups:  id, 1000
#> 
#> Fixed effects:
#>             Estimate Std. Error z value  Pr(>|z|)
#> (Intercept)    0.041    0.05803  0.7065 4.799e-01
#> x              0.971    0.08594 11.2994 1.321e-29
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

## Acknoweledgement

Some parts of the code base for galamm has been derived from internal
functions of [gamm4](https://cran.r-project.org/package=gamm4) (author:
Simon Wood and Fabian Scheipl) and
[lme4](https://cran.r-project.org/package=lme4) (authors: Douglas M.
Bates, Martin Maechler, Ben Bolker, and Steve Walker). Places where this
occurs have been marked with comments in the source code.

## Code of Conduct

Please note that the galamm project is released with a [Contributor Code
of Conduct](https://lcbc-uio.github.io/galamm/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

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

<div id="ref-nealeOpenMxExtendedStructural2016" class="csl-entry">

Neale, Michael C., Michael D. Hunter, Joshua N. Pritikin, Mahsa Zahery,
Timothy R. Brick, Robert M. Kirkpatrick, Ryne Estabrook, Timothy C.
Bates, Hermine H. Maes, and Steven M. Boker. 2016. “OpenMx 2.0: Extended
Structural Equation and Statistical Modeling.” *Psychometrika* 81 (2):
535–49. <https://doi.org/10.1007/s11336-014-9435-8>.

</div>

<div id="ref-rabe-heskethGeneralizedMultilevelStructural2004"
class="csl-entry">

Rabe-Hesketh, Sophia, Anders Skrondal, and Andrew Pickles. 2004.
“Generalized Multilevel Structural Equation Modeling.” *Psychometrika*
69 (2): 167–90. <https://doi.org/10.1007/BF02295939>.

</div>

<div id="ref-rabe-heskethMaximumLikelihoodEstimation2005"
class="csl-entry">

———. 2005. “Maximum Likelihood Estimation of Limited and Discrete
Dependent Variable Models with Nested Random Effects.” *Journal of
Econometrics* 128 (2): 301–23.
<https://doi.org/10.1016/j.jeconom.2004.08.017>.

</div>

<div id="ref-rockwoodEstimatingComplexMeasurement2019"
class="csl-entry">

Rockwood, Nicholas J., and Minjeong Jeon. 2019. “Estimating Complex
Measurement and Growth Models Using the R Package PLmixed.”
*Multivariate Behavioral Research* 54 (2): 288–306.
<https://doi.org/10.1080/00273171.2018.1516541>.

</div>

<div id="ref-rosseelLavaanPackageStructural2012" class="csl-entry">

Rosseel, Yves. 2012. “Lavaan: An R Package for Structural Equation
Modeling.” *Journal of Statistical Software* 48 (May): 1–36.
<https://doi.org/10.18637/jss.v048.i02>.

</div>

<div id="ref-skrondalGeneralizedLatentVariable2004" class="csl-entry">

Skrondal, Anders, and Sophia Rabe-Hesketh. 2004. *Generalized Latent
Variable Modeling*. Interdisciplinary Statistics Series. Boca Raton,
Florida: Chapman and Hall/CRC.

</div>

<div id="ref-sorensenLongitudinalModelingAgeDependent2023"
class="csl-entry">

Sørensen, Øystein, Anders M. Fjell, and Kristine B. Walhovd. 2023.
“Longitudinal Modeling of Age-Dependent Latent Traits with Generalized
Additive Latent and Mixed Models.” *Psychometrika* 88 (2): 456–86.
<https://doi.org/10.1007/s11336-023-09910-z>.

</div>

<div id="ref-woodGeneralizedAdditiveModels2017a" class="csl-entry">

Wood, Simon N. 2017. *Generalized Additive Models: An Introduction with
R*. 2nd ed. Chapman and Hall/CRC.

</div>

</div>
