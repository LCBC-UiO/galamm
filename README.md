
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The galamm package for fitting generalized additive latent and mixed models

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/LCBC-UiO/galamm/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/galamm/actions)
<!-- badges: end -->

galamm is an R package for fitting multivariate generalized additive
mixed models with factor structures. The package is currently under
development, and changes to the API must be expected. If you are looking
for a package for fitting generalized linear mixed models with factor
structures, check out
[PLmixed](https://cran.r-project.org/package=PLmixed).

## Installation

You can install the development version of galamm from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("LCBC-UiO/galamm")
```

## Example

Here is a basic example with three items measuring an underlying latent
trait.

``` r
library(galamm)
#> Loading required package: gamm4
#> Loading required package: Matrix
#> Loading required package: lme4
#> Loading required package: mgcv
#> Loading required package: nlme
#> 
#> Attaching package: 'nlme'
#> The following object is masked from 'package:lme4':
#> 
#>     lmList
#> This is mgcv 1.8-34. For overview type 'help("mgcv-package")'.
#> This is gamm4 0.2-6

# Define factor loading matrix
load.mat <- matrix(c(1, NA, NA), ncol = 1)
dimnames(load.mat) <- list(c("item1", "item2", "item3"), NULL)

# Fit a galamm
mod <- galamm(
  formula = value ~ s(x, by = weight),
  random = ~(1|id),
  data = dat1,
  load.var = "item",
  lambda = load.mat,
  factor = "weight",
  optim_control = list(trace = 3)
  )
#> N = 2, M = 5 machine precision = 2.22045e-16
#> This problem is unconstrained.
#> At X0, 0 variables are exactly at the bounds
#> At iterate     0  f=       954.75  |proj g|=       192.41
#> Bad direction in the line search;
#>    refresh the lbfgs memory and restart the iteration.
#> At iterate    10  f =       774.32  |proj g|=        8.6731
#> Warning: Some predictor variables are on very different scales: consider
#> rescaling

#> Warning: Some predictor variables are on very different scales: consider
#> rescaling

#> Warning: Some predictor variables are on very different scales: consider
#> rescaling

#> Warning: Some predictor variables are on very different scales: consider
#> rescaling

#> Warning: Some predictor variables are on very different scales: consider
#> rescaling

#> Warning: Some predictor variables are on very different scales: consider
#> rescaling

#> Warning: Some predictor variables are on very different scales: consider
#> rescaling

#> Warning: Some predictor variables are on very different scales: consider
#> rescaling

#> Warning: Some predictor variables are on very different scales: consider
#> rescaling

#> Warning: Some predictor variables are on very different scales: consider
#> rescaling
#> At iterate    20  f =        741.3  |proj g|=        11.206
#> 
#> iterations 26
#> function evaluations 81
#> segments explored during Cauchy searches 2
#> BFGS updates skipped 0
#> active bounds at final generalized Cauchy point 0
#> norm of the final projected gradient 0.00105644
#> final function value 741.116
#> 
#> F = 741.116
#> final  value 741.115782 
#> converged
```
