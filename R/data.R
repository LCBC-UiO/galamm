#' Epilepsy Data
#'
#' Longitudinal epilepsy data from
#' \insertCite{leppikControlledStudyProgabide1987;textual}{galamm}. This
#' documentation is based on Chapter 11.3 of
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
#'   weeks preceding entry into the trial.}
#' }
#' @source <http://www.gllamm.org/books/readme.html#11.3>
#' @family datasets
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
#'
#' @family datasets
"mresp"

#' Simulated Mixed Response Data with Heteroscedastic Residuals
#'
#' Mixed response dataset with one set of normally distributed
#' responses and one set of binomially distributed responses. The normally
#' distributed response follow two different residual standard deviations.
#'
#' @format ## `mresp` A data frame with 4000 rows and 5 columns:
#' \describe{
#'   \item{id}{Subject ID.}
#'   \item{x}{Predictor variable.}
#'   \item{y}{Response.}
#'   \item{itemgroup}{Factor variable which equals "a" for the normally
#'   distributed responses and "b" for the binomially distributed response
#'   (with 1 trial).}
#'   \item{grp}{Grouping variable denoting which of the two residual standard
#'   deviations apply. Only relevant for the normally distributed responses.}
#'   \item{isgauss}{Dummy variable indicating whether the observation on the
#'   given line is normally (Gaussian) distributed or not.}
#' }
#'
#' @family datasets
"mresp_hsced"

#' Diet Data
#'
#' Longitudinal epilepsy data from
#' \insertCite{morrisDietHeartPostscript1977;textual}{galamm}. This
#' documentation is based on Chapter 14.2 of
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
#' @family datasets
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
#' @family datasets
#' @references \insertAllCited{}
"hsced"

#' Simulated Data with Measurements of Cognitive Abilities
#'
#' Simulated dataset mimicking the measurement of abilities in three cognitive
#' domains. The latent traits (cognitive ability in a given domain) are based on
#' the functions in \code{mgcv::gamSim}
#' \insertCite{woodGeneralizedAdditiveModels2017a}{galamm}, and depend on the
#' explanatory variable x.
#'
#' @format ## `cognition` A data frame with 14400 rows and 7 columns:
#' \describe{
#'   \item{id}{Subject ID.}
#'   \item{domain}{Factor variable denoting the cognitive domain.}
#'   \item{x}{Explanatory variable.}
#'   \item{timepoint}{Factor variable denoting the timepoint.}
#'   \item{item}{Factor variable denoting the item within the tests of each
#'   cognitive domain.}
#'   \item{trials}{Number of trials, if applicable.}
#'   \item{y}{Response variable. For domain 1 a real number, for domain 2 a
#'       binomially distributed variable based on a single trial, for
#'       domain 3 a real number.}
#' }
#' @family datasets
#' @references \insertAllCited{}
#'
"cognition"

#' Simulated Data with Latent and Observed Covariates Interaction
#'
#' Simulated dataset for use in examples and testing with a latent covariate
#' interacting with an observed covariate.
#'
#' @format ## `latent_covariates` A data frame with 600 rows and 5 columns:
#' \describe{
#'   \item{id}{Subject ID.}
#'   \item{type}{Type of observation in the \code{y} variable.
#'   If it equals \code{"measurement1"} or \code{"measurement2"} then the
#'   observation is a measurement of the latent variable. If it equals
#'   \code{"response"}, then the observation is the actual response.}
#'   \item{x}{Explanatory variable.}
#'   \item{y}{Observed response. Note, this includes both the actual response,
#'   and the measurements of the latent variable, since mathematically they
#'   are all treated as responses.}
#'   \item{response}{Dummy variable indicating whether the given row is a
#'   response or not.}
#' }
#' @family datasets
#'
"latent_covariates"

#' Simulated Longitudinal Data with Latent and Observed Covariates Interaction
#'
#' Simulated dataset for use in examples and testing with a latent covariate
#' interacting with an observed covariate. In this data, each response has been
#' measured six times for each subject.
#'
#' @format ## `latent_covariates_long` A data frame with 800 rows and 5
#' columns:
#' \describe{
#'   \item{id}{Subject ID.}
#'   \item{type}{Type of observation in the \code{y} variable.
#'   If it equals \code{"measurement1"} or \code{"measurement2"} then the
#'   observation is a measurement of the latent variable. If it equals
#'   \code{"response"}, then the observation is the actual response.}
#'   \item{x}{Explanatory variable.}
#'   \item{y}{Observed response. Note, this includes both the actual response,
#'   and the measurements of the latent variable, since mathematically they
#'   are all treated as responses.}
#'   \item{response}{Dummy variable indicating whether the given row is a
#'   response or not.}
#' }
#' @family datasets
#'
"latent_covariates_long"
