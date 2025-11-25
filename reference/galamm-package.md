# galamm: Generalized Additive Latent and Mixed Models

Estimates generalized additive latent and mixed models using maximum
marginal likelihood, as defined in Sorensen et al. (2023)
[doi:10.1007/s11336-023-09910-z](https://doi.org/10.1007/s11336-023-09910-z)
, which is an extension of Rabe-Hesketh and Skrondal (2004)'s unifying
framework for multilevel latent variable modeling
[doi:10.1007/BF02295939](https://doi.org/10.1007/BF02295939) . Efficient
computation is done using sparse matrix methods, Laplace approximation,
and automatic differentiation. The framework includes generalized
multilevel models with heteroscedastic residuals, mixed response types,
factor loadings, smoothing splines, crossed random effects, and
combinations thereof. Syntax for model formulation is close to 'lme4'
(Bates et al. (2015)
[doi:10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01) ) and
'PLmixed' (Rockwood and Jeon (2019)
[doi:10.1080/00273171.2018.1516541](https://doi.org/10.1080/00273171.2018.1516541)
).

## References

Sørensen Ø, Fjell AM, Walhovd KB (2023). “Longitudinal Modeling of
Age-Dependent Latent Traits with Generalized Additive Latent and Mixed
Models.” *Psychometrika*, **88**(2), 456–486. ISSN 1860-0980,
[doi:10.1007/s11336-023-09910-z](https://doi.org/10.1007/s11336-023-09910-z)
.

## See also

Useful links:

- <https://github.com/LCBC-UiO/galamm>

- <https://lcbc-uio.github.io/galamm/>

- Report bugs at <https://github.com/LCBC-UiO/galamm/issues>

## Author

**Maintainer**: Øystein Sørensen <oystein.sorensen@psykologi.uio.no>
([ORCID](https://orcid.org/0000-0003-0724-3542))

Other contributors:

- Douglas Bates \[contributor\]

- Ben Bolker \[contributor\]

- Martin Maechler \[contributor\]

- Allan Leal \[contributor\]

- Fabian Scheipl \[contributor\]

- Steven Walker \[contributor\]

- Simon Wood \[contributor\]
