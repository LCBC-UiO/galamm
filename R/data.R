#' Simulated example dataset with latent binomial responses
#'
#' Simulated example dataset in which a latent construct is measured by
#' three items, at up to four timepoints. Each measurement consists of five
#' trials.
#'
#' @format A dataframe with the following columns:
#' \describe{
#'   \item{id}{factor variable with participant id.}
#'   \item{tp}{factor variable with timepoint id.}
#'   \item{item}{factor variable with item id.}
#'   \item{time}{numeric variable denoting the time for the given measurement.}
#'   \item{y}{integer between 0 and 5, representing the number of successes in the given trial.}
#' }
"latent_response_example"
