# Quantile-quantile plots for galamm objects

Quantile-quantile plots for galamm objects

## Usage

``` r
# S3 method for class 'galamm'
qqmath(x, data = NULL, ...)
```

## Arguments

- x:

  An object of class `galamm`, returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- data:

  Ignored. Required for S3 method compatibility.

- ...:

  Optional parameters passed on to other methods. Currently not used.

## Value

A quantile-quantile plot.

## See also

Other diagnostics:
[`plot.galamm()`](https://lcbc-uio.github.io/galamm/reference/plot.galamm.md)

## Author

This function is derived from `lme4:::qqmath.merMod`, written by Douglas
Bates, Martin Maechler, Ben Bolker, Steve Walker.

## Examples

``` r
## Linear mixed model example from lme4
data("sleepstudy", package = "lme4")
mod <- galamm(Reaction ~ Days + (Days | Subject), data = sleepstudy)
qqmath(mod)

```
