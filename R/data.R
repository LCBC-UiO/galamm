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
#'   \item{lbas}{Logarithm of a quarter of the number of seizures in the eight weeks preceeding entry into the trial.}
#' }
#' @source <http://www.gllamm.org/books/readme.html#11.3>
#' @references \insertAllCited{}
"epilep"


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
#'   \item{age}{Age in years.}
#'   \item{bus}{Dummy variable indicating whether the subject is a bus driver or banking staff.}
#'   \item{y}{Outcome.}
#'   \item{d1, d2}{Dummy variables indicating whether y is measurement of fiber intake at timepoint 1 or 2.}
#'   \item{d3}{Dummy variable indicating whether y is an indicator for coronary heart disease, coded as 0/1.}
#' }
#' @source <http://www.gllamm.org/books/readme.html#14.2>
#' @references \insertAllCited{}
"diet"
