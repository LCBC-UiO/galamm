# Set up smooth term with factor loading

This is a very thin wrapper around
[`mgcv::t2`](https://rdrr.io/pkg/mgcv/man/t2.html). It enables the
specification of loading variables for smooth terms. The last letter
"l", which stands for "loading", has been added to avoid namespace
conflicts with `mgcv` and `gamm4`.

## Usage

``` r
t2l(..., factor = NULL)
```

## Arguments

- ...:

  Arguments passed on to
  [`mgcv::t2`](https://rdrr.io/pkg/mgcv/man/t2.html).

- factor:

  Optional character of length one specifying the loading variable. Case
  sensitive.

## Value

An object of class `xx.smooth.spec`, where `xx` is a basis identifying
code given by the `bs` argument of `t2`. It differs from the smooth
returned by [`mgcv::s`](https://rdrr.io/pkg/mgcv/man/s.html) in that it
has an additional attribute named `"factor"` which specifies any factor
loading which this smooth term should be multiplied with in order to
produce the observed outcome.

## Details

The documentation of the function
[`mgcv::t2`](https://rdrr.io/pkg/mgcv/man/t2.html) should be consulted
for details on how to properly set up smooth terms. In particular, note
that these terms distinguish between ordered and unordered factor terms
in the `by` variable, which can be provided in `...` and is forwarded to
[`mgcv::t2`](https://rdrr.io/pkg/mgcv/man/t2.html).

## References

Wood SN (2003). “Thin Plate Regression Splines.” *Journal of the Royal
Statistical Society. Series B (Statistical Methodology)*, **65**(1),
95–114. ISSN 1369-7412.

Wood SN (2017). *Generalized Additive Models: An Introduction with R*, 2
edition. Chapman and Hall/CRC.

## See also

Other modeling functions:
[`galamm()`](https://lcbc-uio.github.io/galamm/reference/galamm.md),
[`galammObject`](https://lcbc-uio.github.io/galamm/reference/galammObject.md),
[`gfam()`](https://lcbc-uio.github.io/galamm/reference/gfam.md),
[`s()`](https://lcbc-uio.github.io/galamm/reference/sl.md)

## Examples

``` r
# Linear mixed model with factor structures
dat <- subset(cognition, domain == 1 & timepoint == 1)
loading_matrix <- matrix(c(1, NA, NA), ncol = 1)

# Model with four cubic regression splines as basis functions
mod <- galamm(
  formula = y ~ 0 + item + t2l(x, k = 4, factor = "loading"),
  data = dat,
  load_var = "item",
  lambda = loading_matrix,
  factor = "loading"
)

# Model with four thin-plate regression splines as basis functions
mod <- galamm(
  formula = y ~ 0 + item +
    t2l(x, bs = "tp", k = 4, factor = "loading"),
  data = dat,
  load_var = "item",
  lambda = loading_matrix,
  factor = "loading"
)
```
