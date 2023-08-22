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
#' @source <http://www.gllamm.org/books/readme.html#11.3>
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
#'   \item{fiber}{Dummy variable indicating whether y is a fiber measurement
#'   at either timepoint 1 or 2.}
#'   \item{fiber2}{Dummy variable indicating whether y is a fiber measurement
#'   at timepoint 2.}
#' }
#' @source <http://www.gllamm.org/books/readme.html#14.2>
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


#' Simulated Dataset on Lifespan Trajectories of Three Cognitive Domains
#'
#' This is a simulated dataset based on the example in Section 4 of
#' \insertCite{sorensenLongitudinalModelingAgeDependent2023;textual}{galamm}. It
#' consists of data on three cognitive domains, episodic memory, working memory,
#' and executive function, measured by three different tests. Each test consists
#' of multiple measurements. Episodic memory is measured by the California
#' Verbal Learning Test (CVLT)
#' \insertCite{delisCVLTCaliforniaVerbal1987,delisCVLTCaliforniaVerbal2000}{galamm}
#' with responses representing the number of successes in each trial, and
#' integer between 0 and 16. Working memory is measured by digit span backward and
#' forward tests \insertCite{blackburnRevisedAdministrationScoring1959,ostrosky-solisDigitSpanEffect2006}{galamm},
#' also here with the responses being an integer between 0 and 16 representing
#' the number of successes. Finally, executive function was measured by the Stroop
#' test \insertCite{stroopStudiesInterferenceSerial1935}{galamm}, with the response
#' being the time taken to complete the test.
#'
#' @format ## `cognition` A data frame with 40834 rows and 13 columns:
#' \describe{
#'   \item{id}{Subject ID.}
#'   \item{y}{Response value. For the Stroop tests, a standardized decimal number,
#'   for the CVLT and digit span tests, a whole number representing the number of successes.}
#'   \item{trials}{An integer representing the number of trials for each of the CVLT and digit
#'   span tests. For Stroop tests it has the number 16, but this is completely arbitrary, since
#'   it is not used with Gaussian response.}
#'   \item{itemgroup}{One of "cvlt", "digitspan", and "stroop", representing the
#'   group of items to which the current row belongs.}
#'   \item{test}{Name of the test, which belongs to the itemgroup.}
#'   \item{age}{Age at the time of the test.}
#'   \item{age_z}{Standardized age at the time of the test.}
#'   \item{itemgroup_retest}{Grouping variable for specifying retest effects.}
#'   \item{retest}{Dummy variable indicating whether this is a retest or not.}
#'   \item{timepoint}{Integer value representing the timepoint, starting at 1.}
#' }
#' @source <https://github.com/LCBC-UiO/galamm-scripts/>
#' @references \insertAllCited{}
"cognition"
