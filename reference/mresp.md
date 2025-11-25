# Simulated Mixed Response Data

Very basic mixed response dataset with one set of normally distributed
responses and one set of binomially distributed responses.

## Usage

``` r
mresp
```

## Format

### `mresp` A data frame with 4000 rows and 5 columns:

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

## See also

Other datasets:
[`cognition`](https://lcbc-uio.github.io/galamm/reference/cognition.md),
[`diet`](https://lcbc-uio.github.io/galamm/reference/diet.md),
[`epilep`](https://lcbc-uio.github.io/galamm/reference/epilep.md),
[`hsced`](https://lcbc-uio.github.io/galamm/reference/hsced.md),
[`latent_covariates`](https://lcbc-uio.github.io/galamm/reference/latent_covariates.md),
[`latent_covariates_long`](https://lcbc-uio.github.io/galamm/reference/latent_covariates_long.md),
[`lifespan`](https://lcbc-uio.github.io/galamm/reference/lifespan.md),
[`mresp_hsced`](https://lcbc-uio.github.io/galamm/reference/mresp_hsced.md)
