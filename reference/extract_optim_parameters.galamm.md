# Extract parameters from fitted model for use as initial values

This function extracts parameter values from a fitted model object in a
form that can be directly provided as initial values for a new model
fit.

## Usage

``` r
# S3 method for class 'galamm'
extract_optim_parameters(object)
```

## Arguments

- object:

  Object of class `galamm` returned from
  [`galamm`](https://lcbc-uio.github.io/galamm/reference/galamm.md).

## Value

A `list` object containing the following elements:

- `theta` Numerical vector of variance components, i.e., entries of the
  lower Cholesky form of the covariance matrix of random effects.

- `beta` Fixed regression coefficients.

- `lambda` Factor loadings.

- `weights` Weights for heteroscedastic residuals.

## See also

Other optimization functions:
[`galamm_control()`](https://lcbc-uio.github.io/galamm/reference/galamm_control.md)

## Examples

``` r
# Fit linear mixed model with heteroscedastic residuals
mod <- galamm(
  formula = y ~ x + (1 | id),
  dispformula = ~ (1 | item),
  data = hsced
)

# Extract parameters
start <- extract_optim_parameters(mod)

# Fit again using the Nelder-Mead algorithm, using start as initial values:
mod_nm <- galamm(
  formula = y ~ x + (1 | id),
  dispformula = ~ (1 | item),
  data = hsced,
  start = start,
  control = galamm_control(method = "Nelder-Mead")
)
```
