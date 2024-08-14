# galamm version 0.2.1

- Software paper in Multivariate Behavioral Research has been added to the 
  recommended citation.

# galamm version 0.2.0

- The print.summary.galamm() method does no longer print residual percentiles 
  with mixed response models. The version implemented until now had a bug, and 
  it is not clear how to present this information in a useful way.
- Fixed bug causing galamm() to fail with mixed response types, when the first 
  argument to "family" was not "gaussian".
- BREAKING CHANGE: argument "factor", "factor_interaction" and "lambda" to 
  galamm should no longer be enclosed in a list.
- A vignette investigating computational scalability has been added.
- formula.galamm() method has been added, inheriting from stats::formula().
- nobs.galamm() function is now exported.
- na.action argument has been added to galamm().
- input validation has been extended.
- all internal functions have been documented, using the @noRd tag to suppress
  generation of markdown.

# galamm version 0.1.1

- Fixed bug causing galamm to fail on R4.2.3.
- Fixed memory issues in C++ code.
- In the smooth terms sl() and t2(), the argument 'load.var' has been renamed to
  'factor', to correspond with the remaining factor arguments.

# galamm version 0.1.0

Initial version.
