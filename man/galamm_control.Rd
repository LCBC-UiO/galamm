% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/galamm_control.R
\name{galamm_control}
\alias{galamm_control}
\title{Control values for galamm fit}
\usage{
galamm_control(
  optim_control = list(),
  method = c("L-BFGS-B", "Nelder-Mead"),
  maxit_conditional_modes = 10,
  pirls_tol_abs = 0.01,
  reduced_hessian = FALSE
)
}
\arguments{
\item{optim_control}{List containing optimization parameters. If \code{method
  = "L-BFGS-B"} it is passed on to \code{stats::optim}'s \code{control}
argument and if \code{method = "Nelder-Mead"}, it is passed on to
\code{lme4::Nelder_Mead}'s control argument. If not otherwise specified,
and \code{method = "L-BFGS-B"}, the following arguments are set to
non-default values: \code{fnscale = -1} and \code{lmm = 20}.}

\item{method}{Character string defining the algorithm to be used for
maximizing the marginal log-likelihood. The default is \code{"L-BFGS-B"},
which uses the limited memory Broyden-Fletcher-Goldfarb-Shanno algorithm
with box constrained as implemented in \code{stats::optim}. The other
options is \code{"Nelder-Mead"}, which calls the Nelder-Mead algorithm with
box constraints implemented in \code{lme4::Nelder_Mead}. The argument is
case sensitive.}

\item{maxit_conditional_modes}{Maximum number of iterations in penalized
iteratively reweighted least squares algorithm. Ignored if \code{family =
  "gaussian"} for all observations, since then a single step gives the exact
answer.}

\item{pirls_tol_abs}{Absolute convergence criterion for penalized
iteratively reweighted least squares algorithm. Defaults to 0.01, which
means that when the reduction in marginal likelihood between two iterations
is below 0.01, the iterations stop.}

\item{reduced_hessian}{Logical value. Defaults to \code{TRUE}, which means
that the full Hessian matrix at the maximum marginal likelihood solution is
computed. If \code{FALSE}, a reduced Hessian matrix with second order
partial derivatives with respect to fixed regression coefficients and
factor loadings. The latter can help is the full Hessian is not positive
definite.}
}
\value{
Object of class \code{galamm_control}, which typically will be
provided as an argument to \code{\link{galamm}}.
}
\description{
This function can be called for controling the optimization
procedure used when fitting GALAMMs using \code{\link{galamm}}.
}
\examples{
# Define control object with quite a high degree of verbosity (trace = 6)
# and using the last 20 BFGS updates to estimate the Hessian in L-BFGS-B.
control <- galamm_control(optim_control = list(trace = 6, lmm = 20))

}
\references{
\insertRef{batesFittingLinearMixedEffects2015}{galamm}

\insertRef{broydenConvergenceClassDoublerank1970}{galamm}

\insertRef{byrdLimitedMemoryAlgorithm1995}{galamm}

\insertRef{fletcherNewApproachVariable1970}{galamm}

\insertRef{goldfarbFamilyVariablemetricMethods1970}{galamm}

\insertRef{nelderSimplexMethodFunction1965}{galamm}

\insertRef{shannoConditioningQuasiNewtonMethods1970}{galamm}
}
\seealso{
\code{\link[=galamm]{galamm()}}

Other optimization functions: 
\code{\link{extract_optim_parameters.galamm}()}
}
\concept{optimization functions}
