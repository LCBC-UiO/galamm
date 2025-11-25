# Epilepsy Data

Longitudinal epilepsy data from Leppik et al. (1987) . This
documentation is based on Chapter 11.3 of Skrondal and Rabe-Hesketh
(2004) , where the dataset is used.

## Usage

``` r
epilep
```

## Format

### `epilep` A data frame with 236 rows and 7 columns:

- subj:

  Subject ID.

- y:

  Number of seizures.

- treat:

  Dummy variable for treatment group.

- visit:

  Time at visit.

- v4:

  Dummy for visit 4.

- lage:

  Logarithm of age.

- lbas:

  Logarithm of a quarter of the number of seizures in the eight weeks
  preceding entry into the trial.

## Source

<http://www.gllamm.org/books/readme.html#11.3>

## References

Leppik IE, Dreifuss FE, Porter R, Bowman T, Santilli N, Jacobs M, Crosby
C, Cloyd J, Stackman J, Graves N, Sutula T, Welty T, Vickery J, Brundage
R, Gates J, Gumnit RJ, Gutierrez A (1987). “A Controlled Study of
Progabide in Partial Seizures: Methodology and Results.” *Neurology*,
**37**(6), 963–963. ISSN 0028-3878, 1526-632X,
[doi:10.1212/WNL.37.6.963](https://doi.org/10.1212/WNL.37.6.963) .  
  
Skrondal A, Rabe-Hesketh S (2004). *Generalized Latent Variable
Modeling*, Interdisciplinary Statistics Series. Chapman and Hall/CRC,
Boca Raton, Florida.

## See also

Other datasets:
[`cognition`](https://lcbc-uio.github.io/galamm/reference/cognition.md),
[`diet`](https://lcbc-uio.github.io/galamm/reference/diet.md),
[`hsced`](https://lcbc-uio.github.io/galamm/reference/hsced.md),
[`latent_covariates`](https://lcbc-uio.github.io/galamm/reference/latent_covariates.md),
[`latent_covariates_long`](https://lcbc-uio.github.io/galamm/reference/latent_covariates_long.md),
[`lifespan`](https://lcbc-uio.github.io/galamm/reference/lifespan.md),
[`mresp`](https://lcbc-uio.github.io/galamm/reference/mresp.md),
[`mresp_hsced`](https://lcbc-uio.github.io/galamm/reference/mresp_hsced.md)
