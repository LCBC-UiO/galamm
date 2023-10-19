#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags. These may
#' be moved at any time to any other locations in your code. Once addressed,
#' please modify the tag from `@srrstatsTODO` to `@srrstats`, or `@srrstatsNA`,
#' ensuring that references to every one of the following standards remain
#' somewhere within your code. (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
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
#' @srrstats {G5.10} Environment variable GALAMM_EXTENDED_TESTS used. It
#'   can be triggered by adding "run-extended" in the Git commit message, in
#'   case of which it will be run on GitHub Actions.
#' @srrstats {G5.11} No large datasets required for extended tests.
#' @srrstats {G5.11a} No data for extended tests needs to be downloaded.
#' @srrstats {G5.12} No special conditions necessary to run extended tests.
#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G5.4c} Published paper based on data which cannot be shared,
#'   for privacy reasons.
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @noRd
NULL
