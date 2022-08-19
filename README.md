
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The galamm package for fitting generalized additive latent and mixed models

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

galamm is an R package for fitting multivariate generalized additive
mixed models with factor structures. The package is currently under
development, and changes to the API must be expected.

The package uses an interface similar to
[PLmixed](https://cran.r-project.org/package=PLmixed), which fits
generalized linear mixed models with factor structures. However, galamm
allows semiparametric estimations with
[gamm4](https://cran.r-project.org/package=gamm4), and hence more
flexibly allows estimation of nonlinear effects. If you don’t need
semiparametric estimation, on the other hand, your should use definitely
[PLmixed](https://cran.r-project.org/package=PLmixed), as it is a much
more mature package.

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
```
