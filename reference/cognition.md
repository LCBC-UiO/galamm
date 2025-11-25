# Simulated Data with Measurements of Cognitive Abilities

Simulated dataset mimicking the measurement of abilities in three
cognitive domains. The latent traits (cognitive ability in a given
domain) are based on the functions in
[`mgcv::gamSim`](https://rdrr.io/pkg/mgcv/man/gamSim.html) (Wood 2017) ,
and depend on the explanatory variable x.

## Usage

``` r
cognition
```

## Format

### `cognition` A data frame with 14400 rows and 7 columns:

- id:

  Subject ID.

- domain:

  Factor variable denoting the cognitive domain.

- x:

  Explanatory variable.

- timepoint:

  Factor variable denoting the timepoint.

- item:

  Factor variable denoting the item within the tests of each cognitive
  domain.

- trials:

  Number of trials, if applicable.

- y:

  Matrix with two columns. The first column is the response variable:
  for domain 1 a real number, for domain 2 a binomially distributed
  variable based on a single trial, for domain 3 a real number. The
  second column equals 1 if the response is conditionally Gaussian and 2
  if the response is conditionally binomial.

## References

Wood SN (2017). *Generalized Additive Models: An Introduction with R*, 2
edition. Chapman and Hall/CRC.

## See also

Other datasets:
[`diet`](https://lcbc-uio.github.io/galamm/reference/diet.md),
[`epilep`](https://lcbc-uio.github.io/galamm/reference/epilep.md),
[`hsced`](https://lcbc-uio.github.io/galamm/reference/hsced.md),
[`latent_covariates`](https://lcbc-uio.github.io/galamm/reference/latent_covariates.md),
[`latent_covariates_long`](https://lcbc-uio.github.io/galamm/reference/latent_covariates_long.md),
[`lifespan`](https://lcbc-uio.github.io/galamm/reference/lifespan.md),
[`mresp`](https://lcbc-uio.github.io/galamm/reference/mresp.md),
[`mresp_hsced`](https://lcbc-uio.github.io/galamm/reference/mresp_hsced.md)
