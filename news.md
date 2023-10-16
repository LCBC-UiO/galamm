# galamm (development versions)

- formula.galamm() method has been added, inheriting from stats::formula().
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
