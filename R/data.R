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
#'   \item{subj}{Subject ID}
#'   \item{y}{Number of seizures}
#'   \item{treat}{Dummy variable for treatment group}
#'   \item{visit}{Time at visit}
#'   \item{v4}{Dummy for visit 4}
#'   \item{lage}{Logarithm of age}
#'   \item{lbas}{Logarithm of a quarter of the number of seizures in the eight weeks preceeding entry into the trial}
#' }
#' @source <http://www.gllamm.org/books/readme.html#11.3>
#' @references \insertAllCited{}
"epilep"
