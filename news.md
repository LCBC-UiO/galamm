# galamm 0.3.0 (development versions)

- plot.galamm() has been considerably extended.

# galamm 0.3.0

- The expected output of a unit test has been changed.
- A bug in the computation of deviance residuals has been corrected.
- A bug in the computation of Pearson residuals for non-Gaussian models has been
  corrected.
- The plot() function now takes an optional argument for the type of residuals 
  to plot.
- The family_mapping argument for mixed response types has been deprecated. Use
  the gfam() function instead.
- Argument weights to galamm has been renamed to dispformula. The old name 
  weights will work for a while, but a deprecation warning will be issued.
- Improved tests.
- Improved error messages.
- Argument load.var to galamm has been renamed to load_var. The old name 
  load_var will work for a while, but a deprecation warning will be issued.

# galamm 0.2.3

- A bug causing heteroscedastic models with more than two groups to fail has
  been solved. Thanks to Espen Moen Eilertsen for reporting the issue.
- Have rerun all vignettes. There are some minor changes in the formatting of
  summary(model), due to changes in lme4.

# galamm 0.2.2

- Internal fix to avoid warning about uninitialized variables.
- New function response() returns the response used for fitting the model.
- New print.galamm() function which is an alias for print(summary(galamm_obj)).
- Rank deficient fixed effect matrices now leads to an error. Previously only
  a message was written that the redundant columns were dropped.


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
- In the smooth terms sl() and t2(), the argument 'load_var' has been renamed to
  'factor', to correspond with the remaining factor arguments.

# galamm version 0.1.0

Initial version.
