#' Epilepsy Data
#'
#' Longitudinal epilepsy data from
#' \insertCite{leppikControlledStudyProgabide1987;textual}{galamm}. This
#' documenation is based on Chapter 11.3 of
#' \insertCite{skrondalGeneralizedLatentVariable2004;textual}{galamm}, where
#' the dataset is used.
#'
#' @format ## `epilep` A data frame with 236 rows and 7 columns:
#' \describe{
#'   \item{subj}{Subject ID.}
#'   \item{y}{Number of seizures.}
#'   \item{treat}{Dummy variable for treatment group.}
#'   \item{visit}{Time at visit.}
#'   \item{v4}{Dummy for visit 4.}
#'   \item{lage}{Logarithm of age.}
#'   \item{lbas}{Logarithm of a quarter of the number of seizures in the eight
#'   weeks preceeding entry into the trial.}
#' }
#' @source <http://www.gllamm.org/books/readme.html>
#' @references \insertAllCited{}
"epilep"

#' Simulated Mixed Response Data
#'
#' Very basic mixed response dataset with one set of normally distributed
#' responses and one set of binomially distributed responses.
#'
#' @format ## `mresp` A data frame with 4000 rows and 5 columns:
#' \describe{
#'   \item{id}{Subject ID.}
#'   \item{x}{Predictor variable.}
#'   \item{y}{Response.}
#'   \item{itemgroup}{Factor variable which equals "a" for the normally
#'   distributed responses and "b" for the binomially distributed response
#'   (with 1 trial).}
#' }
"mresp"

#' Diet Data
#'
#' Longitudinal epilepsy data from
#' \insertCite{morrisDietHeartPostscript1977;textual}{galamm}. This documenation
#' is based on Chapter 14.2 of
#' \insertCite{skrondalGeneralizedLatentVariable2004;textual}{galamm}, where the
#' dataset is used. See also
#' \insertCite{rabe-heskethCorrectingCovariateMeasurement2003;textual}{galamm}.
#'
#' @format ## `diet` A data frame with 236 rows and 7 columns:
#' \describe{
#'   \item{id}{Subject ID.}
#'   \item{age}{Age (standardized).}
#'   \item{bus}{Dummy variable indicating whether the subject is a bus
#'   driver or banking staff.}
#'   \item{item}{Integer indicating whether the outcome is fiber intake at
#'   time 1 (item = 1), fiber intake at time 2 (item = 2), or coronary heart
#'   disease (item = 3).}
#'   \item{y}{Outcome.}
#'   \item{chd}{Dummy variable indicating whether y is an indicator for
#'   coronary heart disease, coded as 0/1.}
#'   \item{fiber}{Dummary variable indicating whether y is a fiber measurement
#'   at either timepoint 1 or 2.}
#'   \item{fiber2}{Dummary variable indicating whether y is a fiber measurement
#'   at timepoint 2.}
#' }
#' @source <http://www.gllamm.org/books/readme.html>
#' @references \insertAllCited{}
"diet"

#' Example Data with Heteroscedastic Residuals
#'
#' Simulated dataset with residual standard deviation that varies between
#' items.
#'
#' @format ## `hsced` A data frame with 1200 rows and 5 columns:
#' \describe{
#'   \item{id}{Subject ID.}
#'   \item{age}{Timepoint.}
#'   \item{item}{Item indicator.}
#'   \item{x}{Explanatory variable}
#'   \item{y}{Outcome.}
#' }
#' @references \insertAllCited{}
"hsced"
