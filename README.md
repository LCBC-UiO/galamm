
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The galamm package for fitting generalized additive latent and mixed models

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/LCBC-UiO/galamm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LCBC-UiO/galamm/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

galamm is an R package for fitting multivariate generalized additive
mixed models with factor structures. The package is currently under
development, and changes to the API must be expected.

The package uses an interface similar to
[PLmixed](https://cran.r-project.org/package=PLmixed), which fits
generalized linear mixed models with factor structures. However, galamm
allows semiparametric estimations with
[gamm4](https://cran.r-project.org/package=gamm4), and hence more
flexibly allows estimation of nonlinear effects.

The plan for the package is to provide tool for fast and scalable
estimation of generalized linear latent and mixed models (GLLAMMs)
(Rabe-Hesketh, Skrondal, and Pickles 2004) and their semiparametric
extensions which we have termed generalized additive latent and mixed
models (GALAMMs) (Sørensen, Fjell, and Walhovd 2023). This is already
possible to some extent, using the function `marginal_likelihood`, as is
documented in the vignette on maximum likelihood estimation. To obtain
this, we combine sparse matrix computations with RcppEigen (Bates and
Eddelbuettel 2013) and automatic differentiation using the C++ library
[autodiff](https://autodiff.github.io/) (Leal 2018).

## Installation

You can install the development version of galamm from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("LCBC-UiO/galamm")
```

## Example

For now, please refer to [this
repository](https://github.com/LCBC-UiO/galamm-scripts) for code that
can be used to reproduce the results of Sørensen, Fjell, and Walhovd
(2023).

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-batesFastElegantNumerical2013" class="csl-entry">

Bates, Douglas, and Dirk Eddelbuettel. 2013. “Fast and Elegant Numerical
Linear Algebra Using the RcppEigen Package.” *Journal of Statistical
Software* 52 (February): 1–24. <https://doi.org/10.18637/jss.v052.i05>.

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

<div id="ref-sorensenLongitudinalModelingAgeDependent2023"
class="csl-entry">

Sørensen, Øystein, Anders M. Fjell, and Kristine B. Walhovd. 2023.
“Longitudinal Modeling of Age-Dependent Latent Traits with Generalized
Additive Latent and Mixed Models.” *Psychometrika* 88 (2): 456–86.
<https://doi.org/10.1007/s11336-023-09910-z>.

</div>

</div>
