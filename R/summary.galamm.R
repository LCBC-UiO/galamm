#' Summary method for GALAMM fits
#'
#' Summary information for a fitted \code{\link{galamm}} object.
#'
#' @param object A fitted \code{\link{galamm}} object.
#' @param ... Other arguments.
#' @rdname summary.galamm
#'
#' @return Summary information.
#' @author Much of the code is based on the \code{summary} function of the
#' \code{PLmixed} package, written by Minjeong Jeon and Nicholas Rockwood.
#' @export
#'
#' @examples
#' # Example dataset with three measurements of a latent response
#' # Define loading matrix
#' load.mat <- matrix(c(1, NA, NA), ncol = 1)
#' dimnames(load.mat) <- list(c("item1", "item2", "item3"), NULL)
#' \dontrun{
#' mod <- galamm(
#'   formula = y ~ s(x, by = weight),
#'   random = ~(1|id),
#'   data = dat1,
#'   load_var = "item",
#'   lambda = load.mat,
#'   factor = "weight",
#'   lambda_init = c(2, .4)
#'   )
#' x <- summary(mod)
#'   }
#'
summary.galamm <- function(object, ...){

  ## Most of this code comes from the PLmixed package by Nicholas Rockwood and Minjeong Jeon
  ## https://cran.r-project.org/package=PLmixed

  details <- list("nobs" = stats::nobs(object$gamm4$mer),
                  "ngrps" = lme4::ngrps(object$gamm4$mer))

  fit.stat <- list("AIC" = stats::AIC(object$gamm4$mer) + 2*length(object$loadings_estimated),
                   "BIC" = (stats::BIC(object$gamm4$mer)
                            + length(object$loadings_estimated)*(log(stats::nobs(object$gamm4$mer)))),
                   "logLik" = object$log_likelihood,
                   "deviance" = stats::deviance(object$gamm4$mer),
                   "df.resid" = stats::df.residual(object$gamm4$mer) - object$loadings_estimated)

  return.object <- list("Formula" = Reduce(paste, deparse(object$formula)),
                        "Family" = stats::family(object$gamm4$mer),
                        "Fit" = fit.stat,
                        "Optim Iterations" = object$iterations,
                        "Lambda" = object$lambda_est,
                        "load_var" = object$load_var,
                        "Random Effects" = object$random_effects,
                        "Fixed Effects" = object$fixed_effects,
                        "Details" = details,
                        "Residuals" = stats::residuals(object$gamm4$mer),
                        "Scaled Residuals" = stats::residuals(object$gamm4$mer, scale = TRUE),
                        "Param" = object$lambda_est)
  class(return.object) <- append("summary.galamm", class(return.object))
  return.object
}


#' Print method for summary outputs
#'
#' Print the summary output of a fitted \code{\link{galamm}} object.
#'
#' @param x an object of class \code{\link{galamm}}
#' @param digits minimal number of significant digits.
#' @param ... Additional arguments.
#' @rdname print.summary.galamm
#' @export
#'

print.summary.galamm <- function(x, digits = 4, ...){
  ## Most of this code comes from the PLmixed package by Nicholas Rockwood and Minjeong Jeon
  ## https://cran.r-project.org/package=PLmixed

  object <- x
  cat("Generalized additive latent and mixed model fit with profile likelihood\n")
  cat("Formula: ", object$Formula, "\n")
  lme4::.prt.family(object$Family)
  cat("\n")
  aictab <- unlist(object$Fit)
  lme4::.prt.aictab(aictab, digits = 2)
  cat("\n")
  lme4::.prt.resids(object$'Scaled Residuals', digits = digits)

  lam <- object$Lambda
  cat("Lambda: ", object$load_var, "\n")
  print(lam, digits = digits, zero.print = ".", na.print = ".")
  cat("\n")

  lme4::.prt.VC(object$`Random Effects`, comp = c("Var", "Std.Dev."), digits = digits)
  lme4::.prt.grps(ngrps = object$'Details'$'ngrps', nobs = object$'Details'$'nobs')
  cat(" \nFixed effects: \n")
  print(object$'Fixed Effects', digits = digits)
  cat("\n")
  cat("Optim Iterations: ", object$'Optim Iterations', "\n")
  return(NULL)
}
