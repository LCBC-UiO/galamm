#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags. These may
#' be moved at any time to any other locations in your code. Once addressed,
#' please modify the tag from `@srrstatsTODO` to `@srrstats`, or `@srrstatsNA`,
#' ensuring that references to every one of the following standards remain
#' somewhere within your code. (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose FALSE
#'
#' @srrstats {G1.2} Life cycle statement is in the file .github/CONTRIBUTING.md
#' @srrstats {G1.5} No performance claims made in associated publication. Might
#'   be added in the future, if a publication is made based on this package.
#' @srrstats {G2.4e} No explicit conversion from factor to other types is done.
#' @srrstats {G2.8} Not directly relevant.
#' @srrstats {G2.9} I'm not aware of any conversions in which information is
#'   lost.
#' @srrstats {G2.11} We do not rely on class attributes of columns, except for
#'   factors.
#' @srrstats {G2.12} List columns are not supported.
#' @srrstats {G2.14b} Ignoring missing data does not make sense mathematically,
#'   so option \code{na.action = "na.omit"} will cause an error, through
#'   match.arg().
#' @srrstats {G2.14c} Replacing missing values with properly imputed values is a
#'   modeling step in itself. For this to be valid, a model has to be fitted on
#'   each imputed dataset, and the estimates need to be combined. Users who want
#'   to do this, would need to set up the infrastructure themselves, potentially
#'   using the \code{mice} package.
#' @srrstats {G3.0} No floating point numbers are compared for equality.
#' @srrstats {G3.1} The \code{stats::cov} function is not used. In the models
#'   supported by \code{galamm}, the only easily available covariance matrix is
#'   the inverse of the Hessian of the marginal log-likelihood at the local
#'   optimum. This is the asymptotic covariance matrix. Other ways of computing
#'   covariance matrices are not within scope.
#' @srrstats {G3.1a} No alternative covariance methods are supported.
#' @srrstats {G4.0} Output written to local files is not supported.
#' @srrstatsTODO {G5.2} *Appropriate error and warning behaviour of all
#'   functions should be explicitly demonstrated through tests. In particular,*
#' @srrstatsTODO {G5.2a} *Every message produced within R code by `stop()`,
#'   `warning()`, `message()`, or equivalent should be unique*
#' @srrstatsTODO {G5.2b} *Explicit tests should demonstrate conditions which
#'   trigger every one of those messages, and should compare the result with
#'   expected values.*
#' @srrstatsTODO {G5.3} *For functions which are expected to return objects
#'   containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the
#'   absence of any such values in return objects should be explicitly tested.*
#' @srrstatsTODO {G5.4} **Correctness tests** *to test that statistical
#'   algorithms produce expected results to some fixed test data sets
#'   (potentially through comparisons using binding frameworks such as
#'   [RStata](https://github.com/lbraglia/RStata)).*
#' @srrstatsTODO {G5.4a} *For new methods, it can be difficult to separate out
#'   correctness of the method from the correctness of the implementation, as
#'   there may not be reference for comparison. In this case, testing may be
#'   implemented against simple, trivial cases or against multiple
#'   implementations such as an initial R implementation compared with results
#'   from a C/C++ implementation.*
#' @srrstatsTODO {G5.4b} *For new implementations of existing methods,
#'   correctness tests should include tests against previous implementations.
#'   Such testing may explicitly call those implementations in testing,
#'   preferably from fixed-versions of other software, or use stored outputs
#'   from those where that is not possible.*
#' @srrstatsTODO {G5.4c} *Where applicable, stored values may be drawn from
#'   published paper outputs when applicable and where code from original
#'   implementations is not available*
#' @srrstatsTODO {G5.5} *Correctness tests should be run with a fixed random
#'   seed*
#' @srrstatsTODO {G5.6} **Parameter recovery tests** *to test that the
#'   implementation produce expected results given data with known properties.
#'   For instance, a linear regression algorithm should return expected
#'   coefficient values for a simulated data set generated from a linear model.*
#' @srrstatsTODO {G5.6a} *Parameter recovery tests should generally be expected
#'   to succeed within a defined tolerance rather than recovering exact values.*
#' @srrstatsTODO {G5.6b} *Parameter recovery tests should be run with multiple
#'   random seeds when either data simulation or the algorithm contains a random
#'   component. (When long-running, such tests may be part of an extended,
#'   rather than regular, test suite; see G4.10-4.12, below).*
#' @srrstatsTODO {G5.7} **Algorithm performance tests** *to test that
#'   implementation performs as expected as properties of data change. For
#'   instance, a test may show that parameters approach correct estimates within
#'   tolerance as data size increases, or that convergence times decrease for
#'   higher convergence thresholds.*
#' @srrstatsTODO {G5.8} **Edge condition tests** *to test that these conditions
#'   produce expected behaviour such as clear warnings or errors when confronted
#'   with data with extreme properties including but not limited to:*
#' @srrstatsTODO {G5.8a} *Zero-length data*
#' @srrstatsTODO {G5.8b} *Data of unsupported types (e.g., character or complex
#'   numbers in for functions designed only for numeric data)*
#' @srrstatsTODO {G5.8c} *Data with all-`NA` fields or columns or all identical
#'   fields or columns*
#' @srrstatsTODO {G5.8d} *Data outside the scope of the algorithm (for example,
#'   data with more fields (columns) than observations (rows) for some
#'   regression algorithms)*
#' @srrstatsTODO {G5.9} **Noise susceptibility tests** *Packages should test for
#'   expected stochastic behaviour, such as through the following conditions:*
#' @srrstatsTODO {G5.9a} *Adding trivial noise (for example, at the scale of
#'   `.Machine$double.eps`) to data does not meaningfully change results*
#' @srrstatsTODO {G5.9b} *Running under different random seeds or initial
#'   conditions does not meaningfully change results*
#' @srrstatsTODO {G5.10} *Extended tests should included and run under a common
#'   framework with other tests but be switched on by flags such as as a
#'   `<MYPKG>_EXTENDED_TESTS="true"` environment variable.* - The extended tests
#'   can be then run automatically by GitHub Actions for example by adding the
#'   following to the `env` section of the workflow:
#' @srrstatsTODO {G5.11} *Where extended tests require large data sets or other
#'   assets, these should be provided for downloading and fetched as part of the
#'   testing workflow.*
#' @srrstatsTODO {G5.11a} *When any downloads of additional data necessary for
#'   extended tests fail, the tests themselves should not fail, rather be
#'   skipped and implicitly succeed with an appropriate diagnostic message.*
#' @srrstatsTODO {G5.12} *Any conditions necessary to run extended tests such as
#'   platform requirements, memory, expected runtime, and artefacts produced
#'   that may need manual inspection, should be described in developer
#'   documentation such as a `CONTRIBUTING.md` or `tests/README.md` file.*
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @noRd
NULL
