# Simulated Longitudinal Data with Latent and Observed Covariates Interaction

Simulated dataset for use in examples and testing with a latent
covariate interacting with an observed covariate. In this data, each
response has been measured six times for each subject.

## Usage

``` r
latent_covariates_long
```

## Format

### `latent_covariates_long` A data frame with 800 rows and 5

columns:

- id:

  Subject ID.

- type:

  Type of observation in the `y` variable. If it equals `"measurement1"`
  or `"measurement2"` then the observation is a measurement of the
  latent variable. If it equals `"response"`, then the observation is
  the actual response.

- x:

  Explanatory variable.

- y:

  Observed response. Note, this includes both the actual response, and
  the measurements of the latent variable, since mathematically they are
  all treated as responses.

- response:

  Dummy variable indicating whether the given row is a response or not.

## See also

Other datasets:
[`cognition`](https://lcbc-uio.github.io/galamm/reference/cognition.md),
[`diet`](https://lcbc-uio.github.io/galamm/reference/diet.md),
[`epilep`](https://lcbc-uio.github.io/galamm/reference/epilep.md),
[`hsced`](https://lcbc-uio.github.io/galamm/reference/hsced.md),
[`latent_covariates`](https://lcbc-uio.github.io/galamm/reference/latent_covariates.md),
[`lifespan`](https://lcbc-uio.github.io/galamm/reference/lifespan.md),
[`mresp`](https://lcbc-uio.github.io/galamm/reference/mresp.md),
[`mresp_hsced`](https://lcbc-uio.github.io/galamm/reference/mresp_hsced.md)
