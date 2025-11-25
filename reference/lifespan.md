# Simulated Dataset with Lifespan Trajectories of Three Cognitive Domains

This dataset is simulated based on the data used in Section 4.1 of
Sørensen et al. (2023) .

## Usage

``` r
lifespan
```

## Format

### `lifespan` A data frame with 54,457 rows and 7 columns:

- id:

  Subject ID.

- domain:

  Cognitive domain being measured. One of `"epmem"` for episodic memory,
  `"wmem"` for working memory and `"execfun"` for executive
  function/speed.

- timepoint:

  Integer indicating the timepoint number.

- age:

  Age of participant at the timepoint.

- test:

  The particular test at this observation.

- y:

  Response. For `"epmem"` and `"wmem"` this is the number of successes
  in 16 trials. For `"execfun"` it is the time in seconds to complete
  the task.

- retest:

  Integer indicating whether the participant has taken the test at a
  previous timepoint.

- domainepmem:

  Dummy variable for `domain=="epmem"`.

- domainwmem:

  Dummy variable for `domain=="wmem"`.

- domainexecfun:

  Dummy variable for `domain=="execfun"`.

## References

Sørensen Ø, Fjell AM, Walhovd KB (2023). “Longitudinal Modeling of
Age-Dependent Latent Traits with Generalized Additive Latent and Mixed
Models.” *Psychometrika*, **88**(2), 456–486. ISSN 1860-0980,
[doi:10.1007/s11336-023-09910-z](https://doi.org/10.1007/s11336-023-09910-z)
.

## See also

Other datasets:
[`cognition`](https://lcbc-uio.github.io/galamm/reference/cognition.md),
[`diet`](https://lcbc-uio.github.io/galamm/reference/diet.md),
[`epilep`](https://lcbc-uio.github.io/galamm/reference/epilep.md),
[`hsced`](https://lcbc-uio.github.io/galamm/reference/hsced.md),
[`latent_covariates`](https://lcbc-uio.github.io/galamm/reference/latent_covariates.md),
[`latent_covariates_long`](https://lcbc-uio.github.io/galamm/reference/latent_covariates_long.md),
[`mresp`](https://lcbc-uio.github.io/galamm/reference/mresp.md),
[`mresp_hsced`](https://lcbc-uio.github.io/galamm/reference/mresp_hsced.md)
