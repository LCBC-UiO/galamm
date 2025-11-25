# Optimization

``` r
library(galamm)
```

The purpose of this vignette is to describe the optimization procedure
used by `galamm`, and what kind of tools one can use in the case of
convergence issues.

### High-Level Overview

The optimization procedure used by `galamm` is described in Section 3 of
Sørensen, Fjell, and Walhovd
([2023](#ref-sorensenLongitudinalModelingAgeDependent2023)). It consists
of two steps:

- In the inner loop, the marginal likelihood is evaluated at a given set
  of parameters. The marginal likelihood is what you obtain by
  integrating out the random effects, and this integration is done with
  the Laplace approximation. The Laplace approximation yields a large
  system of equations that needs to be solved iteratively, except in the
  case with conditionally Gaussian responses and unit link function, for
  which a single step is sufficient to solve the system. When written in
  matrix-vector form, this system of equations will in most cases have
  an overwhelming majority of zeros, and to avoid wasting memory and
  time on storing and multiplying zero, we use sparse matrix methods.

- In the outer loop, we try to find the parameters that maximize the
  marginal likelihood. For each new set of parameters, the whole
  procedure in the inner loop has to be repeated. By default, we use the
  limited memory Broyden-Fletcher-Goldfarb-Shanno algorithm with box
  constraints ([Byrd et al. 1995](#ref-byrdLimitedMemoryAlgorithm1995)),
  abbreviated L-BFGS-B. In particular, we use the implementation in R’s
  [`optim()`](https://rdrr.io/r/stats/optim.html) function, which is
  obtained by setting `method = "L-BFGS-B"`. L-BFGS-B requires first
  derivatives, and these are obtained by automatic differentiation
  ([Skaug 2002](#ref-skaugAutomaticDifferentiationFacilitate2002)). In
  most use cases of `galamm`, we also use constraints on some of the
  parameters, e.g., to ensure that variances are non-negative. As an
  alternative, the Nelder-Mead algorithm with box constraints ([Bates et
  al. 2015](#ref-batesFittingLinearMixedEffects2015); [Nelder and Mead
  1965](#ref-nelderSimplexMethodFunction1965)) from `lme4` is also
  available. Since the Nelder-Mead algorithm is derivative free,
  automatic differentiation is not used in this case, except for
  computing the Hessian matrix at the final step.

At convergence, the Hessian matrix of second derivatives is computed
exactly, again using automatic differentiation. The inverse of this
matrix is the covariance matrix of the parameter estimates, and is used
to compute Wald type confidence intervals.

### Modifying the L-BFGS-B algorithm

We will illustrate some ways of modifying the optimization procedure
with the covariate measurement model example shown in the [vignette on
models with mixed response
types](https://lcbc-uio.github.io/galamm/articles/mixed_response.html).
Here we start by simply setting up what we need to fit the model.

``` r
loading_matrix <- matrix(c(1, 1, NA), ncol = 1)
families <- gfam(list(gaussian, binomial))
formula <- y ~ 0 + chd + (age * bus):chd + fiber +
  (age * bus):fiber + fiber2 + (0 + loading | id)
```

Fitting the model with default arguments yields a warning when we look
at the summary object.

``` r
mod <- galamm(
  formula = formula,
  data = diet,
  family = families,
  factor = "loading",
  load_var = "item",
  lambda = loading_matrix
)

summary(mod)
#> Warning in vcov.galamm(object, parm = "lambda"): Rank deficient Hessian matrix.Could not compute covariance matrix.
#> Warning in vcov.galamm(object, "beta"): Rank deficient Hessian matrix.Could not compute covariance matrix.
#> GALAMM fit by maximum marginal likelihood.
#> Formula: formula
#>    Data: diet
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>   2837.6   2892.9  -1406.8  12529.3      730 
#> 
#> Lambda:
#>         loading SE
#> lambda1   1.000  .
#> lambda2   1.000  .
#> lambda3  -2.026  .
#> 
#> Random effects:
#>  Groups Name    Variance Std.Dev.
#>  id     loading 0        0       
#> Number of obs: 742, groups:  id, 333
#> 
#> Fixed effects:
#>               Estimate Std. Error z value Pr(>|z|)
#> chd           -1.78692         NA      NA       NA
#> fiber         17.96184         NA      NA       NA
#> fiber2        -0.64927         NA      NA       NA
#> chd:age        0.06682         NA      NA       NA
#> chd:bus       -0.06882         NA      NA       NA
#> fiber:age     -0.20480         NA      NA       NA
#> fiber:bus     -1.69601         NA      NA       NA
#> chd:age:bus   -0.04934         NA      NA       NA
#> fiber:age:bus  0.16097         NA      NA       NA
```

In this case, we can increase the amount of information provided by
`optim`, with the `trace` argument. To avoid getting too much output, we
also reduce the number of iterations. We set the `control` argument as
follows:

``` r
control <- galamm_control(optim_control = list(maxit = 5, trace = 3, REPORT = 1))
```

Here, `maxit = 5` means that we take at most 5 iterations, `trace = 3`
means that we want more information from L-BFGS-B, and `REPORT= = 1`
means that we want L-BFGS-B to report information at each step it takes.
We provide this object to the `control` argument in `galamm`, and rerun
the model:

``` r
mod <- galamm(
  formula = formula,
  data = diet,
  family = families,
  factor = "loading",
  load_var = "item",
  lambda = loading_matrix,
  control = control
)
#> N = 11, M = 20 machine precision = 2.22045e-16
#> At X0, 0 variables are exactly at the bounds
#> At iterate     0  f=         2148  |proj g|=       122.68
#> At iterate     1  f =       2132.1  |proj g|=        275.51
#> At iterate     2  f =       2100.1  |proj g|=         193.7
#> At iterate     3  f =       1975.6  |proj g|=        177.11
#> At iterate     4  f =       1923.2  |proj g|=         165.7
#> At iterate     5  f =       1898.8  |proj g|=        83.839
#> At iterate     6  f =       1887.9  |proj g|=        49.147
#> final  value 1887.871646 
#> stopped after 6 iterations
vcov(mod)
#> Warning in vcov.galamm(mod): Rank deficient Hessian matrix.Could not compute covariance matrix.
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#>  [1,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [2,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [3,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [4,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [5,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [6,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [7,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [8,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [9,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
```

Since what we did was simply to turn in more reporting, it is no
surprise that the Hessian is still rank deficient, but from the output,
it is also clear that there are no obvious errors, like values that
diverge to infinity. The latter may also happen from time to time.

By default, L-BFGS-B uses the last 5 evaluations of the gradient to
approximate the Hessian that is used during optimization (not to be
confused with the exact Hessian compute with automatic differentiation
after convergence). We try to increase this to 25, and see if that makes
a difference. This is done with the `lmm` argument. We also reduce the
amount of reporting to be every 10th step, and avoid setting the maximum
number of iterations, which means that
[`optim()`](https://rdrr.io/r/stats/optim.html)’s default option is
used.

``` r
control <- galamm_control(optim_control = list(trace = 3, REPORT = 10, lmm = 25))
```

It is clear that neither this solved the issue.

``` r
mod <- galamm(
  formula = formula,
  data = diet,
  family = families,
  factor = "loading",
  load_var = "item",
  lambda = loading_matrix,
  control = control
)
#> N = 11, M = 25 machine precision = 2.22045e-16
#> At X0, 0 variables are exactly at the bounds
#> At iterate     0  f=         2148  |proj g|=       122.68
#> At iterate    10  f =       1770.3  |proj g|=        30.656
#> At iterate    20  f =       1467.2  |proj g|=        11.286
#> At iterate    30  f =         1413  |proj g|=        4.3102
#> 
#> iterations 38
#> function evaluations 44
#> segments explored during Cauchy searches 39
#> BFGS updates skipped 0
#> active bounds at final generalized Cauchy point 1
#> norm of the final projected gradient 0.001689
#> final function value 1406.8
#> 
#> F = 1406.8
#> final  value 1406.801104 
#> converged
vcov(mod)
#> Warning in vcov.galamm(mod): Rank deficient Hessian matrix.Could not compute covariance matrix.
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#>  [1,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [2,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [3,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [4,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [5,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [6,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [7,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [8,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
#>  [9,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
```

Looking at the model output again, we see that the random effect
variance is estimated to be exactly zero.

``` r
summary(mod)
#> Warning in vcov.galamm(object, parm = "lambda"): Rank deficient Hessian matrix.Could not compute covariance matrix.
#> Warning in vcov.galamm(object, "beta"): Rank deficient Hessian matrix.Could not compute covariance matrix.
#> GALAMM fit by maximum marginal likelihood.
#> Formula: formula
#>    Data: diet
#> Control: control
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>   2837.6   2892.9  -1406.8  12529.3      730 
#> 
#> Lambda:
#>         loading SE
#> lambda1   1.000  .
#> lambda2   1.000  .
#> lambda3  -1.922  .
#> 
#> Random effects:
#>  Groups Name    Variance Std.Dev.
#>  id     loading 0        0       
#> Number of obs: 742, groups:  id, 333
#> 
#> Fixed effects:
#>               Estimate Std. Error z value Pr(>|z|)
#> chd           -1.78673         NA      NA       NA
#> fiber         17.96179         NA      NA       NA
#> fiber2        -0.64904         NA      NA       NA
#> chd:age        0.06685         NA      NA       NA
#> chd:bus       -0.06894         NA      NA       NA
#> fiber:age     -0.20479         NA      NA       NA
#> fiber:bus     -1.69628         NA      NA       NA
#> chd:age:bus   -0.04937         NA      NA       NA
#> fiber:age:bus  0.16092         NA      NA       NA
```

These types of obviously wrong zero variance estimates are well-known
for users of mixed models ([Hodges
2013](#ref-hodgesRichlyParameterizedLinear2013)). We see if increasing
the initial value for the variance parameter solves the issue. This is
done with the `start` argument to `galamm`. The start argument requires
a named list, with optional arguments `theta`, `beta`, `lambda`, and
`weights`, giving initial values for each of these groups of parameters.
In this case `theta` is the standard deviation of the random effect, and
we increase it to 10 to see what happens. By default, the initial value
equals 1.

``` r
mod <- galamm(
  formula = formula,
  data = diet,
  family = families,
  factor = "loading",
  load_var = "item",
  lambda = loading_matrix,
  start = list(theta = 10),
  control = control
)
#> N = 11, M = 25 machine precision = 2.22045e-16
#> At X0, 0 variables are exactly at the bounds
#> At iterate     0  f=       2827.6  |proj g|=       123.31
#> At iterate    10  f =       1764.5  |proj g|=        103.86
#> At iterate    20  f =       1623.6  |proj g|=        131.81
#> At iterate    30  f =       1447.9  |proj g|=        75.096
#> At iterate    40  f =       1400.5  |proj g|=        35.591
#> At iterate    50  f =       1373.1  |proj g|=        3.3359
#> At iterate    60  f =       1372.2  |proj g|=     0.0016541
#> 
#> iterations 60
#> function evaluations 72
#> segments explored during Cauchy searches 61
#> BFGS updates skipped 0
#> active bounds at final generalized Cauchy point 0
#> norm of the final projected gradient 0.00165407
#> final function value 1372.16
#> 
#> F = 1372.16
#> final  value 1372.160386 
#> converged
```

Now we see that the model converged and that the Hessian is no longer
rank deficient.

``` r
summary(mod)
#> GALAMM fit by maximum marginal likelihood.
#> Formula: formula
#>    Data: diet
#> Control: control
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>   2768.3   2823.6  -1372.2   2002.9      730 
#> 
#> Lambda:
#>         loading      SE
#> lambda1  1.0000       .
#> lambda2  1.0000       .
#> lambda3 -0.1339 0.05121
#> 
#> Random effects:
#>  Groups Name    Variance Std.Dev.
#>  id     loading 23.64    4.862   
#> Number of obs: 742, groups:  id, 333
#> 
#> Fixed effects:
#>               Estimate Std. Error  z value   Pr(>|z|)
#> chd           -1.91525    0.27229 -7.03373  2.011e-12
#> fiber         17.94849    0.48686 36.86601 1.620e-297
#> fiber2         0.22402    0.41783  0.53614  5.919e-01
#> chd:age        0.06615    0.05931  1.11531  2.647e-01
#> chd:bus       -0.02895    0.34355 -0.08427  9.328e-01
#> fiber:age     -0.21204    0.10090 -2.10135  3.561e-02
#> fiber:bus     -1.68303    0.63721 -2.64123  8.261e-03
#> chd:age:bus   -0.04999    0.06507 -0.76822  4.424e-01
#> fiber:age:bus  0.16818    0.11223  1.49854  1.340e-01
```

### Optimization with the Nelder-Mead algorithm

The Nelder-Mead algorithm is turned on by setting
`method = "Nelder-Mead"` when calling
[`galamm_control()`](https://lcbc-uio.github.io/galamm/reference/galamm_control.md).
We also turn on reporting every 20th function evaluation by setting
`verbose = 1`:

``` r
control <- galamm_control(
  optim_control = list(verbose = 1),
  method = "Nelder-Mead"
  )
```

We provide the estimates obtained with the L-BFGS-B algorithm as initial
values. For this we can use the convenience function
`extract_optim_parameters`:

``` r
start <- extract_optim_parameters(mod)
```

We now fit the model, providing the initial values to the `start`
argument.

``` r
mod_nm <- galamm(
  formula = formula,
  data = diet,
  family = families,
  factor = "loading",
  load_var = "item",
  lambda = loading_matrix,
  control = control,
  start = start
)
#> (NM) 20: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 40: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 60: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 80: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 100: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 120: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 140: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 160: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 180: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 200: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 220: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 240: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 260: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 280: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 300: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 320: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 340: f = 1372.16 at    1.84246   -1.91525    17.9485   0.224017   0.066146 -0.0289499  -0.212035   -1.68303 -0.0499864   0.168178  -0.133909
#> (NM) 360: f = 1372.16 at    1.84247   -1.91525    17.9485   0.223968  0.0661412  -0.028921   -0.21203   -1.68308 -0.0499804   0.168172  -0.133908
#> (NM) 380: f = 1372.16 at    1.84247   -1.91525    17.9485   0.223979  0.0661419 -0.0289297  -0.212034   -1.68304 -0.0499815   0.168174  -0.133909
#> (NM) 400: f = 1372.16 at    1.84247   -1.91525    17.9485   0.223972   0.066143 -0.0289282  -0.212032   -1.68306 -0.0499827   0.168173  -0.133909
#> (NM) 420: f = 1372.16 at    1.84246   -1.91525    17.9485   0.223982  0.0661428 -0.0289291   -0.21203   -1.68305 -0.0499825    0.16817   -0.13391
```

The summary output shows that Nelder-Mead found exactly the same optimum
in this particular case, which is not surprising given the intial values
that we provided.

``` r
summary(mod_nm)
#> GALAMM fit by maximum marginal likelihood.
#> Formula: formula
#>    Data: diet
#> Control: control
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>   2768.3   2823.6  -1372.2   2002.9      730 
#> 
#> Lambda:
#>         loading      SE
#> lambda1  1.0000       .
#> lambda2  1.0000       .
#> lambda3 -0.1339 0.05121
#> 
#> Random effects:
#>  Groups Name    Variance Std.Dev.
#>  id     loading 23.64    4.862   
#> Number of obs: 742, groups:  id, 333
#> 
#> Fixed effects:
#>               Estimate Std. Error  z value   Pr(>|z|)
#> chd           -1.91525    0.27229 -7.03373  2.011e-12
#> fiber         17.94850    0.48686 36.86601 1.620e-297
#> fiber2         0.22398    0.41783  0.53606  5.919e-01
#> chd:age        0.06614    0.05931  1.11526  2.647e-01
#> chd:bus       -0.02893    0.34355 -0.08422  9.329e-01
#> fiber:age     -0.21203    0.10090 -2.10130  3.561e-02
#> fiber:bus     -1.68304    0.63721 -2.64124  8.260e-03
#> chd:age:bus   -0.04998    0.06507 -0.76814  4.424e-01
#> fiber:age:bus  0.16817    0.11223  1.49847  1.340e-01
```

### Implementation Details

At a given set of parameters, the marginal likelihood is evaluated
completely in C++. For solving the penalized iteratively reweighted
least squares problem arising due to the Laplace approximation, we use
sparse matrix methods from the Eigen C++ template library through the
`RcppEigen` package ([Bates and Eddelbuettel
2013](#ref-batesFastElegantNumerical2013)). In order to keep track of
the derivatives throughout this iterative process, we use the [autodiff
library](https://autodiff.github.io/) ([Leal
2018](#ref-lealAutodiffModernFast2018)). However, since `autodiff`
natively only supports dense matrix operations with `Eigen`, we have
extended this library so that it also supports sparse matrix operations.
This modified version of the `autodiff` library can be found at
`inst/include/autodiff/`.

In order to maximize the marginal likelihood, we currently rely on the
[`optim()`](https://rdrr.io/r/stats/optim.html) function in R. To make
use of the fact that both the marginal likelihood value itself and first
derivatives are returned from the C++ function, we use memoisation,
provided by the `memoise` package ([Wickham et al.
2021](#ref-wickhamMemoiseMemoisationFunctions2021)). However, the
optimization process still involves copying all model data between R and
C++ for each new set of parameters. This is potentially an efficiency
bottleneck with large datasets, although with the limited profiling that
has been done so far, it seems like the vast majority of the computation
time is spent actually solving the penalized iteratively reweighted
least squares problem in C++.

### Future Improvements

We aim to perform also the outer optimization loop in C++, to avoid
copying data back and forth between R and C++ during optimization. This
requires finding an off-the-shelf optimization routine which is as good
as the L-BFGS-B implementation provided by
[`optim()`](https://rdrr.io/r/stats/optim.html), and which plays well
with `autodiff`.

In addition, the current implementation uses only forward mode automatic
differentiation. In the future, we aim to add backward mode as an
option, as this might turn out to be more efficient for problems with a
large number of variables.

## References

Bates, Douglas M, and Dirk Eddelbuettel. 2013. “Fast and Elegant
Numerical Linear Algebra Using the RcppEigen Package.” *Journal of
Statistical Software* 52 (February): 1–24.
<https://doi.org/10.18637/jss.v052.i05>.

Bates, Douglas M, Martin Mächler, Ben Bolker, and Steve Walker. 2015.
“Fitting Linear Mixed-Effects Models Using Lme4.” *Journal of
Statistical Software* 67 (1): 1–48.
<https://doi.org/10.18637/jss.v067.i01>.

Byrd, Richard H., Peihuang Lu, Jorge Nocedal, and Ciyou Zhu. 1995. “A
Limited Memory Algorithm for Bound Constrained Optimization.” *SIAM
Journal on Scientific Computing* 16 (5): 1190–1208.
<https://doi.org/10.1137/0916069>.

Hodges, James S. 2013. *Richly Parameterized Linear Models Additive,
Time Series, and Spatial Models Using Random Effects*. 1st ed. Chapman &
Hall/CRC Texts in Statistical Science. Chapman & Hall.

Leal, Allan M. M. 2018. “Autodiff, a Modern, Fast and Expressive C++
Library for Automatic Differentiation.”

Nelder, J. A., and R. Mead. 1965. “A Simplex Method for Function
Minimization.” *The Computer Journal* 7 (4): 308–13.
<https://doi.org/10.1093/comjnl/7.4.308>.

Skaug, Hans J. 2002. “Automatic Differentiation to Facilitate Maximum
Likelihood Estimation in Nonlinear Random Effects Models.” *Journal of
Computational and Graphical Statistics* 11 (2): 458–70.

Sørensen, Øystein, Anders M. Fjell, and Kristine B. Walhovd. 2023.
“Longitudinal Modeling of Age-Dependent Latent Traits with Generalized
Additive Latent and Mixed Models.” *Psychometrika* 88 (2): 456–86.
<https://doi.org/10.1007/s11336-023-09910-z>.

Wickham, Hadley, Jim Hester, Winston Chang, Kirill Müller, and Daniel
Cook. 2021. *Memoise: ’Memoisation’ of Functions*. Manual.
