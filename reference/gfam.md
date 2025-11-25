# Grouped families

This function is inspired by a function of the same name in the `mgcv`
package (Wood 2017) , and supports setting up mixed response type
models. When using this function, the response variable must be
supported as a two-column matrix, in which the first column contains the
response and the second column contains an index mapping the response to
the list elements provided in the argument `fl` to this function.

## Usage

``` r
gfam(fl)
```

## Arguments

- fl:

  A list of families. Currently `gaussian`, `binomial`, and `poisson`
  with canonical link functions are supported.

## Value

An object of class "galamm_extended_family".

## References

Wood SN (2017). *Generalized Additive Models: An Introduction with R*, 2
edition. Chapman and Hall/CRC.

## See also

Other modeling functions:
[`galamm()`](https://lcbc-uio.github.io/galamm/reference/galamm.md),
[`galammObject`](https://lcbc-uio.github.io/galamm/reference/galammObject.md),
[`s()`](https://lcbc-uio.github.io/galamm/reference/sl.md),
[`t2()`](https://lcbc-uio.github.io/galamm/reference/t2l.md)
