# Control values for galamm fit

This function can be called for controling the optimization procedure
used when fitting GALAMMs using
[`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

## Usage

``` r
galamm_control(
  optim_control = list(),
  method = c("L-BFGS-B", "Nelder-Mead"),
  maxit_conditional_modes = 10,
  pirls_tol_abs = 0.01,
  reduced_hessian = FALSE
)
```

## Arguments

- optim_control:

  List containing optimization parameters. If `method = "L-BFGS-B"` it
  is passed on to [`stats::optim`](https://rdrr.io/r/stats/optim.html)'s
  `control` argument and if `method = "Nelder-Mead"`, it is passed on to
  [`lme4::Nelder_Mead`](https://rdrr.io/pkg/lme4/man/Nelder_Mead.html)'s
  control argument. If not otherwise specified, and
  `method = "L-BFGS-B"`, the following arguments are set to non-default
  values: `fnscale = -1` and `lmm = 20`.

- method:

  Character string defining the algorithm to be used for maximizing the
  marginal log-likelihood. The default is `"L-BFGS-B"`, which uses the
  limited memory Broyden-Fletcher-Goldfarb-Shanno algorithm with box
  constrained as implemented in
  [`stats::optim`](https://rdrr.io/r/stats/optim.html). The other
  options is `"Nelder-Mead"`, which calls the Nelder-Mead algorithm with
  box constraints implemented in
  [`lme4::Nelder_Mead`](https://rdrr.io/pkg/lme4/man/Nelder_Mead.html).
  The argument is case sensitive.

- maxit_conditional_modes:

  Maximum number of iterations in penalized iteratively reweighted least
  squares algorithm. Ignored if `family = "gaussian"` for all
  observations, since then a single step gives the exact answer.

- pirls_tol_abs:

  Absolute convergence criterion for penalized iteratively reweighted
  least squares algorithm. Defaults to 0.01, which means that when the
  reduction in marginal likelihood between two iterations is below 0.01,
  the iterations stop.

- reduced_hessian:

  Logical value. Defaults to `TRUE`, which means that the full Hessian
  matrix at the maximum marginal likelihood solution is computed. If
  `FALSE`, a reduced Hessian matrix with second order partial
  derivatives with respect to fixed regression coefficients and factor
  loadings. The latter can help is the full Hessian is not positive
  definite.

## Value

Object of class `galamm_control`, which typically will be provided as an
argument to
[`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

## References

Bates DM, Mächler M, Bolker B, Walker S (2015). “Fitting Linear
Mixed-Effects Models Using Lme4.” *Journal of Statistical Software*,
**67**(1), 1–48. ISSN 1548-7660,
[doi:10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01) .

BROYDEN CG (1970). “The Convergence of a Class of Double-rank
Minimization Algorithms 1. General Considerations.” *IMA Journal of
Applied Mathematics*, **6**(1), 76–90. ISSN 0272-4960,
[doi:10.1093/imamat/6.1.76](https://doi.org/10.1093/imamat/6.1.76) .

Byrd RH, Lu P, Nocedal J, Zhu C (1995). “A Limited Memory Algorithm for
Bound Constrained Optimization.” *SIAM Journal on Scientific Computing*,
**16**(5), 1190–1208. ISSN 1064-8275,
[doi:10.1137/0916069](https://doi.org/10.1137/0916069) .

Fletcher R (1970). “A New Approach to Variable Metric Algorithms.” *The
Computer Journal*, **13**(3), 317–322. ISSN 0010-4620,
[doi:10.1093/comjnl/13.3.317](https://doi.org/10.1093/comjnl/13.3.317) .

Goldfarb D (1970). “A Family of Variable-Metric Methods Derived by
Variational Means.” *Mathematics of Computation*, **24**(109), 23–26.
ISSN 0025-5718, 1088-6842,
[doi:10.1090/S0025-5718-1970-0258249-6](https://doi.org/10.1090/S0025-5718-1970-0258249-6)
.

Nelder JA, Mead R (1965). “A Simplex Method for Function Minimization.”
*The Computer Journal*, **7**(4), 308–313. ISSN 0010-4620,
[doi:10.1093/comjnl/7.4.308](https://doi.org/10.1093/comjnl/7.4.308) .

Shanno DF (1970). “Conditioning of Quasi-Newton Methods for Function
Minimization.” *Mathematics of Computation*, **24**(111), 647–656. ISSN
0025-5718, 1088-6842,
[doi:10.1090/S0025-5718-1970-0274029-X](https://doi.org/10.1090/S0025-5718-1970-0274029-X)
.

## See also

[`galamm()`](https://lcbc-uio.github.io/galamm/reference/galamm.md)

Other optimization functions:
[`extract_optim_parameters.galamm()`](https://lcbc-uio.github.io/galamm/reference/extract_optim_parameters.galamm.md)

## Examples

``` r
# Define control object with quite a high degree of verbosity (trace = 6)
# and using the last 20 BFGS updates to estimate the Hessian in L-BFGS-B.
control <- galamm_control(optim_control = list(trace = 6, lmm = 20))
```
