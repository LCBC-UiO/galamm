#' Add factor variables as data columns
#'
#' @param load.var The \code{load.var} argument provided to
#'   \code{\link{galamm}}.
#' @param lambda The \code{lambda} argument provided to \code{\link{galamm}}.
#' @param factor The \code{factor} argument provided to \code{\link{galamm}}.
#' @param data The \code{data} argument provided to \code{\link{galamm}}, after
#'   some tests have been applied to validate the input data.
#'
#' @return A list with the following two elements:
#' * A modified version of \code{data} with one extra column for each
#'   \code{factor}.
#' * An updated version of the \code{lambda} list of matrices, with \code{NA}s
#'   replaced by \code{1}s.
#' @noRd
#'
#' @examples
#' data(KYPSsim, package = "PLmixed")
#' loading_matrix <- list(rbind(c(1, 0), c(NA, 0), c(NA, 1), c(NA, NA)))
#' factors <- list(c("ms", "hs"))
#' load.var <- "time"
#' res <- setup_factor(load.var, loading_matrix, factors, KYPSsim)
#'
#' # Columns "ms" and "hs" have now been added:
#' head(res$data)
#' # NA's in the original formulation have been replaced by 1's:
#' res$lambda
setup_factor <- function(load.var, lambda, factor, data) {
  if (!is.null(load.var) && (!is.character(load.var) || length(load.var) != 1)) {
    stop("load.var must be a character of length one.")
  }

  parameter_index <- 2

  for (lv in load.var) {
    eval(parse(text = paste0("data$", lv, "<- factor(data$", lv, ")")))
  }

  for (i in seq_along(factor)) {
    lv <- load.var[[i]]
    lambda[[i]][is.na(lambda[[i]])] <-
      seq(from = parameter_index, length.out = sum(is.na(lambda[[i]])))
    colnames(lambda[[i]]) <- factor[[i]]

    if (any(factor[[i]] %in% colnames(data))) {
      stop("Factor already a column in data.")
    }
    for (j in seq_along(factor[[i]])) {
      if (length(unique(data[, lv])) != length(lambda[[i]][, j])) {
        stop(
          "lambda matrix must contain one row ",
          "for each element in load.var"
        )
      }
      eval(parse(text = paste("data$", factor[[i]][[j]], "<-1")))
      rows_to_zero <-
        data[, lv] %in% levels(data[, lv])[lambda[[i]][, j] == 0]
      eval(
        parse(
          text =
            paste("data$", factor[[i]][[j]], "[rows_to_zero] <- 0")
        )
      )
    }
    parameter_index <- max(lambda[[i]]) + 1
  }

  list(data = data, lambda = lambda)
}

#' Create list of family objects
#'
#' Takes the \code{family} argument returns it in function call form.
#'
#' @param family Argument \code{family} provided to \code{\link{galamm}}.
#'
#' @return A list of family objects.
#' @noRd
#'
#' @examples
#' # Providing a character returns a function call
#' setup_family("binomial")
#'
#' # Providing a function name returns a function call
#' setup_family(binomial)
#'
#' # Providing a function call returns a function call
#' setup_family(binomial())
#'
#' # The same logic extends to lists, as are relevant in mixed response models
#' setup_family(c("gaussian", "binomial"))
#' setup_family(list("gaussian", binomial))
#' setup_family(list(gaussian(), binomial))
setup_family <- function(family) {
  if (length(family) == 1 || inherits(family, "family")) family <- list(family)

  lapply(family, function(f) {
    if (is.character(f)) {
      return(eval(parse(text = f))())
    } else if (is.function(f)) {
      return(f())
    } else {
      return(f)
    }
  })
}

#' Create the response object
#'
#' @param family_list A list of families returned from \code{setup_family}.
#' @param family_mapping The argument \code{family_mapping} provided to
#'   \code{\link{galamm}}.
#' @param data A data frame.
#' @param gobj A list object returned from the internal function \code{gamm4}.
#'
#' @return A matrix with responses and number of trials.
#' @noRd
#'
setup_response_object <- function(family_list, family_mapping, data, gobj) {
  response_obj <- matrix(nrow = nrow(gobj$lmod$X), ncol = 2)

  for (i in seq_along(family_list)) {
    f <- family_list[[i]]
    mf <- stats::model.frame(lme4::nobars(gobj$fake.formula),
      data = data[family_mapping == i, ]
    )
    mr <- stats::model.response(mf)

    if (f$family == "binomial" && !is.null(dim(mr))) {
      trials <- rowSums(mr)
    } else {
      trials <- rep(1, sum(family_mapping == i))
    }
    if (is.matrix(mr)) {
      response <- mr[, 1, drop = TRUE]
    } else {
      response <- mr
    }
    response_obj[family_mapping == i, ] <-
      cbind(response = response, trials = trials)
  }
  response_obj
}

#' Find constant term in log-likelihood
#'
#' Likelihood functions may have a constant term not involving parameters,
#' which is required in the final value, but does not need to be computed at
#' each iteration. This function hence precomputes this constant term.
#'
#' @param family_txt Character vector defining the families. Each element should
#' be one \code{"gaussian"}, \code{"binomial"}, or \code{"poisson"}.
#' @param family_mapping Argument \code{family_mapping} provided to
#'   \code{\link{galamm}}.
#' @param y A numeric vector giving the response.
#' @param trials Number of trials. When irrelevant, should be given as a vector
#' of ones.
#'
#' @return The constant term in the loglikelihood function, one for each family.
#' @noRd
#'
#' @examples
#' # Binomial
#' y1 <- rbinom(10, size = 3, prob = .5)
#' find_k("binomial", rep(1, 10), y1, rep(3, 10))
#'
#' # Poisson
#' y2 <- rpois(10, lambda = 1)
#' find_k("poisson", rep(1, 10), y2, rep(1, 10))
#'
#' # Binomial and Poisson
#' find_k(
#'   c("binomial", "poisson"), c(rep(1, 10), rep(2, 10)),
#'   c(y1, y2), c(rep(3, 10), rep(1, 10))
#' )
#'
#' # For Gaussian, the constant is always zero
#' find_k("gaussian", rep(1, 10), rnorm(10), rep(1, 10))
find_k <- function(family_txt, family_mapping, y, trials) {
  k <- numeric(length(family_txt))
  for (i in seq_along(k)) {
    if (family_txt[[i]] == "gaussian") {
      k[[i]] <- 0
    } else if (family_txt[[i]] == "binomial") {
      trials0 <- trials[family_mapping == i]
      y0 <- y[family_mapping == i]
      k[[i]] <-
        sum(lgamma(trials0 + 1) - lgamma(y0 + 1) - lgamma(trials0 - y0 + 1))
    } else if (family_txt[[i]] == "poisson") {
      trials0 <- trials[family_mapping == i]
      y0 <- y[family_mapping == i]
      k[[i]] <- -sum(lgamma(y0 + 1))
    }
  }
  k
}

#' Set initial values for galamm parameters
#'
#' @param gobj List object with model information
#' @param start Argument \code{start} provided to \code{\link{galamm}}
#'   containing user-defined initial values.
#' @param beta_inds Indices of fixed effect regression coefficients.
#' @param lambda_inds Indices of factor loadings.
#' @param weights_inds Indices of weights.
#'
#' @return A numeric vector with initial values for all parameters.
#' @noRd
#'
set_initial_values <- function(
    gobj, start, beta_inds, lambda_inds, weights_inds) {
  if (length(start) > 0 &&
    any(!names(start) %in% c("beta", "theta", "lambda", "weights"))) {
    stop("Unknown names in initial value list.")
  }

  theta_init <- if (!is.null(start$theta)) {
    if (length(start$theta) != length(gobj$lmod$reTrms$theta)) {
      stop("Wrong number of elements in start$theta")
    }
    start$theta
  } else {
    gobj$lmod$reTrms$theta
  }
  beta_init <- if (!is.null(start$beta)) {
    if (length(start$beta) != length(beta_inds)) {
      stop("Wrong number of elements in start$beta")
    }
    start$beta
  } else {
    rep(0, length(beta_inds))
  }
  lambda_init <- if (!is.null(start$lambda)) {
    if (length(start$lambda) != length(lambda_inds)) {
      stop("Wrong number of elements in start$lambda")
    }
    start$lambda
  } else {
    rep(1, length(lambda_inds))
  }
  weights_init <- if (!is.null(start$weights)) {
    if (length(start$weights) != length(weights_inds)) {
      stop("Wrong number of elements in start$weights")
    }
    start$weights
  } else {
    rep(1, length(weights_inds))
  }
  c(theta_init, beta_init, lambda_init, weights_init)
}

#' Manually added release questions
#'
#' When running \code{devtools::release()} for submitting to CRAN, questions
#' here are asked.
#'
#' @noRd
#'
release_questions <- function() {
  "Did you re-build the vignettes using `rebuild-long-running-vignette.R`?"
}
