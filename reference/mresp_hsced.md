# Simulated Mixed Response Data with Heteroscedastic Residuals

Mixed response dataset with one set of normally distributed responses
and one set of binomially distributed responses. The normally
distributed response follow two different residual standard deviations.

## Usage

``` r
mresp_hsced
```

## Format

### `mresp_hsced` A data frame with 4000 rows and 5 columns:

- id:

  Subject ID.

- x:

  Predictor variable.

- y:

  Matrix whose first column is the response and whose second column
  equals 1 if the observation is conditionally normally distributed and
  2 if the observation is conditionally binomially distributed.

- itemgroup:

  Factor variable which equals "a" for the normally distributed
  responses and "b" for the binomially distributed response (with 1
  trial).

- grp:

  Grouping variable denoting which of the two residual standard
  deviations apply. Only relevant for the normally distributed
  responses.

- isgauss:

  Dummy variable indicating whether the observation on the given line is
  normally (Gaussian) distributed or not.

## See also

Other datasets:
[`cognition`](https://lcbc-uio.github.io/galamm/reference/cognition.md),
[`diet`](https://lcbc-uio.github.io/galamm/reference/diet.md),
[`epilep`](https://lcbc-uio.github.io/galamm/reference/epilep.md),
[`hsced`](https://lcbc-uio.github.io/galamm/reference/hsced.md),
[`latent_covariates`](https://lcbc-uio.github.io/galamm/reference/latent_covariates.md),
[`latent_covariates_long`](https://lcbc-uio.github.io/galamm/reference/latent_covariates_long.md),
[`lifespan`](https://lcbc-uio.github.io/galamm/reference/lifespan.md),
[`mresp`](https://lcbc-uio.github.io/galamm/reference/mresp.md)
