
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

This is a basic example which shows you how to solve a common problem:

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
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
