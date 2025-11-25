# Diet Data

Longitudinal epilepsy data from Morris et al. (1977) . This
documentation is based on Chapter 14.2 of Skrondal and Rabe-Hesketh
(2004) , where the dataset is used. See also Rabe-Hesketh et al. (2003)
.

## Usage

``` r
diet
```

## Format

### `diet` A data frame with 236 rows and 7 columns:

- id:

  Subject ID.

- age:

  Age (standardized).

- bus:

  Dummy variable indicating whether the subject is a bus driver or
  banking staff.

- item:

  Integer indicating whether the outcome is fiber intake at time 1 (item
  = 1), fiber intake at time 2 (item = 2), or coronary heart disease
  (item = 3).

- y:

  Matrix with two column. The first column is the outcome, and the
  second column an index which equals 1 if the response is conditionally
  Gaussian and 2 if the response is conditionally binomial.

- chd:

  Dummy variable indicating whether y is an indicator for coronary heart
  disease, coded as 0/1.

- fiber:

  Dummy variable indicating whether y is a fiber measurement at either
  timepoint 1 or 2.

- fiber2:

  Dummy variable indicating whether y is a fiber measurement at
  timepoint 2.

## Source

<http://www.gllamm.org/books/readme.html#14.2>

## References

Morris JN, Marr JW, Clayton DG (1977). “Diet and Heart: A Postscript.”
*Br Med J*, **2**(6098), 1307–1314. ISSN 0007-1447, 1468-5833,
[doi:10.1136/bmj.2.6098.1307](https://doi.org/10.1136/bmj.2.6098.1307)
.  
  
Rabe-Hesketh S, Pickles A, Skrondal A (2003). “Correcting for Covariate
Measurement Error in Logistic Regression Using Nonparametric Maximum
Likelihood Estimation.” *Statistical Modelling*, **3**(3), 215–232. ISSN
1471-082X,
[doi:10.1191/1471082X03st056oa](https://doi.org/10.1191/1471082X03st056oa)
.  
  
Skrondal A, Rabe-Hesketh S (2004). *Generalized Latent Variable
Modeling*, Interdisciplinary Statistics Series. Chapman and Hall/CRC,
Boca Raton, Florida.

## See also

Other datasets:
[`cognition`](https://lcbc-uio.github.io/galamm/reference/cognition.md),
[`epilep`](https://lcbc-uio.github.io/galamm/reference/epilep.md),
[`hsced`](https://lcbc-uio.github.io/galamm/reference/hsced.md),
[`latent_covariates`](https://lcbc-uio.github.io/galamm/reference/latent_covariates.md),
[`latent_covariates_long`](https://lcbc-uio.github.io/galamm/reference/latent_covariates_long.md),
[`lifespan`](https://lcbc-uio.github.io/galamm/reference/lifespan.md),
[`mresp`](https://lcbc-uio.github.io/galamm/reference/mresp.md),
[`mresp_hsced`](https://lcbc-uio.github.io/galamm/reference/mresp_hsced.md)
