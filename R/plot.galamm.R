#' @title Diagnostic plots for galamm objects
#'
#' @description This function provides diagnostic plots for models fitted with
#'   [galamm()]. See the [residuals.galamm()] function for definition of the
#'   residuals being used.
#'
#' @param x An object of class \code{galamm} returned from \code{\link{galamm}}.
#' @param form An option formula specifying the desired type of plot.
#'   Conditioning variables are specified with a vertical bar.
#' @param abline An optional numeric vector specifying the intercept and slope
#'   of a line to be added to the plot.
#' @param ... Optional arguments passed on to the \code{plot} function.
#' @return A plot is displayed.
#' @export
#'
#' @srrstats {G1.4} Function documented with roxygen2.
#' @srrstats {G2.1a} Expected data types provided for all inputs.
#' @srrstats {RE6.0,RE6.2} Default plot method.
#' @srrstats {RE6.1,RE6.3} Not applicable.
#'
#' @seealso [residuals.galamm()] for extracting residuals and [plot()] for the
#'   generic function.
#'
#' @family summary functions
#' @author Douglas Bates, Martin Maechler, Ben Bolker, and Steven Walker, with
#' modifications by Øystein Sørensen.
#'
#' @details The interface of this function is designed to be similar to the
#'   `plot.merMod` function from `lme4`
#'   \insertCite{batesFittingLinearMixedEffects2015}{galamm}.
#'
#' @references \insertAllCited{}
#'
#' @examples
#' ## Linear mixed model example from lme4
#' data("sleepstudy", package = "lme4")
#' mod <- galamm(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'
#' # Diagnostic plot of Pearson residuals versus fitted values
#' plot(mod)
#'
#' # Include a straight line at zero
#' plot(mod, abline = c(0, 0))
#'
#' # Diagnostic plot of Pearson residuals versus fitted values per subject
#' plot(mod, form = resid(., type = "pearson") ~ fitted(.) | Subject)
#'
#' # Residuals plotted against time with a straight line at zero
#' plot(mod, form = resid(., type = "pearson") ~ Days, abline = c(0, 0))
#'
#' # Residuals plotted against time per subject with a straight line at zero
#' plot(mod, form = resid(., type = "pearson") ~ Days | Subject,
#'      abline = c(0, 0))
#'
#' ## Logistic mixed model example from lme4
#' data("cbpp", package = "lme4")
#' mod <- galamm(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'               data = cbpp, family = binomial)
#'
#' # Diagnostic plot using Pearson residuals
#' plot(mod)
#'
#' # Diagnostic plot using deviance residuals
#' plot(mod, resid(., type = "deviance") ~ fitted(.))
#'
#' # Diagnostic plot per herd
#' plot(mod, resid(., type = "deviance") ~ fitted(.) | herd)
#'
#' ## Linear mixed model with factor structures
#' # See vignette on linear mixed models with factor structures for details
#' data(KYPSsim, package = "PLmixed")
#' KYPSsim <- KYPSsim[KYPSsim$sid < 100, ]
#' KYPSsim$time <- factor(KYPSsim$time)
#'
#' loading_matrix <- rbind(c(1, 0), c(NA, 0), c(NA, 1), c(NA, NA))
#' factors <- c("ms", "hs")
#' load_var <- "time"
#' form <- esteem ~ time + (0 + ms | mid) + (0 + hs | hid) + (1 | sid)
#'
#' mod <- galamm(formula = form, data = KYPSsim, factor = factors,
#'               load_var = load_var, lambda = loading_matrix)
#'
#' # Pearson residuals plotted against fitted value
#' plot(mod)
#'
#' # Actual value plotted against fitted value with a line crossing the diagonal
#' plot(mod, form = esteem ~ fitted(.), abline = c(0, 1))
#'
plot.galamm <- function(x, form = resid(., type = "pearson") ~ fitted(.),
                        abline = NULL, ...) {
  fsplit <- split_conditional_formula(form)
  mf <- model.frame(x)
  env <- list2env(list(. = x), parent = asNamespace("stats"))

  if (!is.null(fsplit$resp)) mf$.y <- eval(fsplit$resp, envir = mf, enclos = env)
  mf$.x <- eval(fsplit$main_rhs, envir = mf, enclos = env)

  cond_var_name <- NULL
  if (!is.null(fsplit$cond)) {
    cond_val <- eval(fsplit$cond, envir = mf, enclos = env)
    cond_var_name <- if (is.name(fsplit$cond)) as.character(fsplit$cond) else "cond"
    mf[[cond_var_name]] <- cond_val
  }

  xlab <- deparse1(fsplit$main_rhs)
  ylab <- if (!is.null(fsplit$resp)) deparse1(fsplit$resp) else NULL

  if (is.null(cond_var_name)) {
    args <- list(.y ~ .x, data = mf, xlab = xlab, ylab = ylab, ...)
    do.call("plot", args)
    if(!is.null(abline)) graphics::abline(a = abline[[1]], b = abline[[2]])
  } else {
    lat_form <- as.formula(paste(".y ~ .x |", cond_var_name))

    dotlist <- list(...)

    add_abline_panel <- function(x, y, ...) {
      lattice::panel.xyplot(x, y, ...)
      if (!is.null(abline)) {
        lattice::panel.abline(a = abline[[1]], b = abline[[2]])
      }
    }

    if (!is.null(dotlist$panel)) {
      user_panel <- dotlist$panel
      dotlist$panel <- function(x, y, ...) {
        user_panel(x, y, ...)
        if (!is.null(abline)) {
          lattice::panel.abline(a = abline[1], b = abline[2])
        }
      }
    } else {
      dotlist$panel <- add_abline_panel
    }

    do.call(lattice::xyplot,
            c(list(lat_form, data = mf, xlab = xlab, ylab = ylab), dotlist))
  }
}

deparse1 <- function(expr) paste(deparse(expr, backtick = TRUE), collapse = "")
split_conditional_formula <- function(f) {
  stopifnot(inherits(f, "formula"))
  resp <- f[[2]]
  rhs  <- f[[3]]
  if (is.call(rhs) && identical(rhs[[1]], as.name("|"))) {
    main_rhs <- rhs[[2]]
    cond     <- rhs[[3]]
  } else {
    main_rhs <- rhs
    cond     <- NULL
  }
  list(resp = resp, main_rhs = main_rhs, cond = cond)
}
