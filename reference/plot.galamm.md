# Diagnostic plots for galamm objects

This function provides diagnostic plots for models fitted with
[`galamm()`](https://lcbc-uio.github.io/galamm/reference/galamm.md). See
the
[`residuals.galamm()`](https://lcbc-uio.github.io/galamm/reference/residuals.galamm.md)
function for definition of the residuals being used.

## Usage

``` r
# S3 method for class 'galamm'
plot(x, residuals = c("pearson", "deviance"), ...)
```

## Arguments

- x:

  An object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- residuals:

  Character of length one describing the type of residuals to be
  returned. One of `"pearson"` and `"deviance"`. Argument is case
  sensitive.

- ...:

  Optional arguments passed on to the `plot` function.

## Value

A plot is displayed.

## See also

[`residuals.galamm()`](https://lcbc-uio.github.io/galamm/reference/residuals.galamm.md)
for extracting residuals and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) for the generic
function.

Other summary functions:
[`anova.galamm()`](https://lcbc-uio.github.io/galamm/reference/anova.galamm.md),
[`plot_smooth.galamm()`](https://lcbc-uio.github.io/galamm/reference/plot_smooth.galamm.md),
[`print.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.galamm.md),
[`print.summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.summary.galamm.md),
[`summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/summary.galamm.md)

## Examples

``` r
# Linear mixed model example from lme4
data("sleepstudy", package = "lme4")
mod <- galamm(Reaction ~ Days + (Days | Subject), data = sleepstudy)

# Diagnostic plot
plot(mod)

plot(mod, residuals = "deviance")
```
