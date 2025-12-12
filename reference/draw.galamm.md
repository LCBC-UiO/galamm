# Draw method for galamm objects

This function uses
[`gratia::draw`](https://gavinsimpson.github.io/gratia/reference/draw.html)
to visualize smooth terms. See
[`gratia::draw()`](https://gavinsimpson.github.io/gratia/reference/draw.html)
for details. When `object` is not of class `galamm`, it is forwarded to
[`gratia::draw()`](https://gavinsimpson.github.io/gratia/reference/draw.html).

## Usage

``` r
# S3 method for class 'galamm'
draw(object, ...)
```

## Arguments

- object:

  An object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

- ...:

  Other arguments passed on to
  [`gratia::draw()`](https://gavinsimpson.github.io/gratia/reference/draw.html).

## Value

A ggplot object.

## See also

Other summary functions:
[`anova.galamm()`](https://lcbc-uio.github.io/galamm/reference/anova.galamm.md),
[`plot_smooth.galamm()`](https://lcbc-uio.github.io/galamm/reference/plot_smooth.galamm.md),
[`print.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.galamm.md),
[`print.summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/print.summary.galamm.md),
[`summary.galamm()`](https://lcbc-uio.github.io/galamm/reference/summary.galamm.md)

## Examples

``` r
dat <- subset(cognition, domain == 1 & item == "11")
dat$y <- dat$y[, 1]
mod <- galamm(y ~ s(x) + (1 | id), data = dat)

draw(mod)

```
