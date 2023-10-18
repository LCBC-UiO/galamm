#' Extract names of factor among column names
#'
#' @param ff Character vector with names of candidate factors.
#' @param cnmf Column name, a single character.
#'
#' @return A character vector containing names of the factors which could be
#' found among the column names.
#'
#' @noRd
#'
#' @examples
#' # Column is an interaction a:b:x, so both "a" and "b" should be returned.
#' extract_name(c("a", "b", "c"), "a:b:x")
#'
extract_name <- function(ff, cnmf) {
  unlist(lapply(ff, function(x) {
    m <- regexpr(x, cnmf, fixed = TRUE)
    regmatches(cnmf, m)
  }))
}

#' Make sure mappings to loadings and to covariates have same size
#'
#' This function adjust the mappings \code{lambda_mapping_Zt} and
#' \code{lambda_mapping_Zt_covs} so they match exactly in size and shape. In
#' particular, it flattens \code{lambda_mapping_Zt_covs} wherever its i-th
#' element has more elements than the i-th element of \code{lambda_mapping_Zt}.
#' See the documentation to the function \code{define_factor_mappings} for more
#' details on what the mappings represent.
#'
#' @param lambda_mapping_Zt Mapping between factor loadings and elements of the
#'   sparse matrix representing the random effect covariates.
#' @param lambda_mapping_Zt_covs Mapping between covariates entering in latent
#'   interactions and elements of the sparse metrix representing the random
#'   effect covariates.
#'
#' @return List of adjusted mappings \code{lambda_mapping_Zt} and
#'   \code{lambda_mapping_Zt_covs}.
#' @noRd
#'
#' @examples
#' # List of length 6
#' lambda_mapping_Zt <- list(0L, 1L, 2L, 0L, 1L, 2L)
#' length(lambda_mapping_Zt)
#' # List of length 4
#' lambda_mapping_Zt_covs <- list(0L, list(3L, 4L), 1L, list(3L, 4L))
#' length(lambda_mapping_Zt_covs)
#' # Squeeze them to got two lists of length 6
#' ret <- squeeze_mappings(lambda_mapping_Zt, lambda_mapping_Zt_covs)
#' vapply(ret, length, integer(1))
#'
squeeze_mappings <- function(lambda_mapping_Zt, lambda_mapping_Zt_covs) {
  ind <- 1L
  security_counter <- 1L
  while (TRUE) {
    if (ind > length(lambda_mapping_Zt_covs)) break

    lzt <- lambda_mapping_Zt[[ind]]
    lztcov <- lambda_mapping_Zt_covs[[ind]]

    if (all(is.na(lzt))) {
      if (length(lzt) > 1) {
        lambda_mapping_Zt[[ind]] <- lambda_mapping_Zt[[ind]][-1]
      } else {
        lambda_mapping_Zt <- lambda_mapping_Zt[-ind]
      }
      if (length(lztcov) > 1) {
        lambda_mapping_Zt_covs[[ind]] <- lambda_mapping_Zt_covs[[ind]][-1]
      } else {
        lambda_mapping_Zt_covs <- lambda_mapping_Zt_covs[-ind]
      }
    } else if (length(lzt) != length(lztcov)) {
      lambda_mapping_Zt_covs <- c(
        lambda_mapping_Zt_covs[seq_len(ind - 1L)],
        lztcov[[1]], lztcov[-1],
        lambda_mapping_Zt_covs[
          seq_len(length(lambda_mapping_Zt_covs) - ind) + ind
        ]
      )
      ind <- ind + 1L
    } else {
      ind <- ind + 1L
    }
    security_counter <- security_counter + 1L
    if (security_counter > 1e9) {
      stop("Loop is probably too long")
    }
  }
  list(
    lambda_mapping_Zt = lambda_mapping_Zt,
    lambda_mapping_Zt_covs = lambda_mapping_Zt_covs
  )
}

#' Convenience function for extracting list elements
#'
#' @param input A list of lists.
#' @param element Name of the list element to be extracted.
#' @param fun Function used to merge the extracted list elements. Defaults to
#'   \code{c}.
#' @param recursive Logical passed on to \code{unlist}, indicating whether the
#'   unlisting done to the final result should be recursive or not. Defaults to
#'   \code{TRUE}.
#'
#' @return A list of unwrapped mappings.
#' @noRd
#' @examples
#' input <- list(
#'   l1 = list(a = runif(8), b = runif(1)),
#'   l2 = list(a = runif(10), b = runif(3))
#' )
#' element <- "b"
#' mappingunwrapping(input, "b")
#'
mappingunwrapping <- function(input, element, fun = c, recursive = TRUE) {
  unlist(
    do.call(function(...) {
      mapply(fun, ..., SIMPLIFY = FALSE)
    }, lapply(input, function(x) x[[element]])),
    recursive = recursive, use.names = FALSE
  )
}

#' Extend mapping between factor loadings and random effect matrix
#'
#' When there are latent factor interactions, the mapping
#' \code{lambda_mapping_Zt} in the list returned by
#' \code{define_factor_mappings} needs to be extended, to incorporate both the
#' traditional loadings and the loadings that multiple provided covariates. This
#' function achieves that.
#'
#' @param fi An element of the list \code{factor_interactions} given to
#'   \code{\link{galamm}}.
#'
#' @return A list of lists, elements of which have been extended to correspond
#'   to the number of covariates multiplying the element.
#' @noRd
#'
#' @examples
#' # Example argument providing interaction between latent and observed
#' # covariates
#' fi <- list(~1, ~x, ~ x + I(x^2))
#' # Three new covariates are required, and we here also get which regression
#' # each of them belongs to.
#' extend_lambda(fi)
#'
extend_lambda <- function(fi) {
  extra_lambdas <- list()
  for (k in seq_along(fi)) {
    if (k == 1) {
      current_max <- 0
    } else {
      inds <- seq(from = 1, to = k - 1, by = 1)
      current_max <- max(vapply(extra_lambdas[inds], function(x) {
        if (length(x) == 0) {
          0
        } else {
          max(x)
        }
      }, numeric(1)))
    }

    extra_lambdas[[k]] <-
      seq_along(attr(stats::terms(fi[[k]]), "term.labels")) +
      current_max
  }
  extra_lambdas
}

#' Convenience function for figuring out whether a given factor is among a set
#' of variables
#'
#' @param factor A character vector, element of \code{factor} provided to
#'   \code{\link{galamm}}.
#' @param vars A set of variables, typically column headers.
#'
#' @return A logical, indicating whether any of the names in \code{factor} can
#' be found among \code{vars}.
#'
#' @noRd
#' @examples
#'
#' factor_finder("f1", letters[1:3])
#' factor_finder(c("f1", "ax", "b"), letters[1:3])
#'
factor_finder <- function(factor, vars) {
  vapply(factor, function(x) {
    any(vapply(vars, function(y) {
      any(vapply(x, function(z) grepl(z, y), TRUE))
    }, TRUE))
  }, TRUE)
}

#' Low-level mappings to factor loadings
#'
#' This function defines the low-level mappings between factor loadings given in
#' the \code{lambda} argument to \code{\link{galamm}} and the underlying dense
#' matrix elements in \eqn{X} and sparse matrix elements in \eqn{Z'}. In these
#' mappings, a value \code{-2} means that the corresponding elements should be
#' multiplied be zero, a value \code{-1} means that the corresponding elements
#' should be multiplied be one, and non-negative integers mean that the
#' corresponding elements should be multiplied by the corresponding element
#' of \code{lambda}, with zero-order indexing as in \code{C++}.
#'
#' @param gobj A list element returned from the internal function \code{gamm4}.
#' @param load.var The argument \code{load.var} argument provided to
#'   \code{\link{galamm}}.
#' @param lambda The argument \code{lambda} argument provided to
#'   \code{\link{galamm}}.
#' @param factor The argument \code{factor} argument provided to
#'   \code{\link{galamm}}.
#' @param factor_interactions The argument \code{factor_interactions} argument
#'   provided to \code{\link{galamm}}.
#' @param data A dataframe, which is a modified version of the \code{data}
#'   argument provided to \code{\link{galamm}}.
#'
#' @return A list object with the following elements:
#' * \code{lambda_mapping_X} A list with mappings between factor loadings in
#'   \code{lambda} and elements of the fixed effect model matrix \eqn{X},
#'   in row-major ordering.
#' * \code{lambda_mapping_Zt} A list with mappings between factor loadings in
#'   \code{lambda} and elements of the random effect model matrix \eqn{Z'}.
#'   The i-th element of \code{lambda_mapping_Zt} corresponds to the i-th
#'   element of \code{Zt@x}.
#' * \code{lambda_mapping_Zt_covs} A list with mappings between covariates
#'   and elements of the random effect model matrix \eqn{Z'}.
#'   The i-th element of \code{lambda_mapping_Zt_covs} corresponds to the i-th
#'   element of \code{Zt@x}, and contains the covariates that the i-th element
#'   of \code{lambda_mapping_Zt} should be multiplied with.
#' * \code{lambda} The factor loadings \code{lambda} with updated indices,
#'   corresponding to the values in the mappings.
#'
#' @noRd
define_factor_mappings <- function(
    gobj, load.var, lambda, factor, factor_interactions, data) {

  if (is.null(factor)) {
    return(
      list(
        lambda_mapping_X = integer(),
        lambda_mapping_Zt = integer(),
        lambda_mapping_Zt_covs = integer(),
        lambda = lambda
      )
    )
  }

  vars_in_fixed <- all.vars(gobj$fake.formula[-2])

  # Add fixed part of smooth terms
  vars_in_fixed <- c(vars_in_fixed, unlist(lapply(gobj$G$smooth, function(x) {
    if (x$null.space.dim > 0) {
      x$label
    }
  })))
  vars_in_fixed <- unique(vars_in_fixed)
  factor_in_fixed <- factor_finder(factor, vars_in_fixed)
  X <- gobj$lmod$X

  if (!any(factor_in_fixed)) {
    lambda_mapping_X <- integer()
  } else {
    lambda_mapping_X <- list()

    for (f in seq_along(factor_in_fixed)) {
      if (factor_in_fixed[[f]]) {
        lv <- load.var[[f]]
        mappings <- lapply(seq_len(ncol(X)), function(i) {
          mapping_component <- rep(-1L, nrow(X))
          target_cnm <- colnames(X)[[i]]
          cnms_match <- vapply(
            colnames(lambda[[f]]),
            function(x) grepl(x, target_cnm), logical(1)
          )
          if (any(cnms_match)) {
            ll <- lambda[[f]][, cnms_match, drop = FALSE] - 2L
          } else {
            return(mapping_component)
          }

          ll[data[, lv]]
        })

        lambda_mapping_X[[f]] <- do.call(c, mappings)

        stopifnot(length(lambda_mapping_X[[f]]) == length(X))

      } else {
        lambda_mapping_X[[f]] <- rep(-1L, length(X))
      }
    }
    lambda_mapping_X <- lambda_mapping_X[[1]]
    stopifnot(length(lambda_mapping_X) == length(X))
  }

  vars_in_random <- unique(unlist(gobj$lmod$reTrms$cnms))
  factor_in_random <- factor_finder(factor, vars_in_random)
  Zt <- gobj$lmod$reTrms$Zt

  if (!any(factor_in_random)) {
    lambda_mapping_Zt <- integer()
  }
  if (is.null(factor_interactions)) {
    lambda_mapping_Zt_covs <- integer()
  }

  for (f in seq_along(factor_in_random)) {
    fi <- factor_interactions[[f]]
    cnms <- lapply(gobj$lmod$reTrms$cnms, function(x) x)
    cnms_match <- lapply(cnms, function(cnm) {
      vapply(
        colnames(lambda[[f]]),
        function(x) any(grepl(x, cnm)), TRUE
      )
    })
    deltas <- lapply(gobj$lmod$reTrms$Ztlist, function(x) diff(x@p))

    if (factor_in_random[[f]]) {
      lv <- load.var[[f]]
      mappings <- lapply(seq_along(cnms), function(i) {
        cnm <- cnms[[i]]
        cnm_match <- cnms_match[[i]]
        delta <- deltas[[i]]
        mapping_component <- rep(NA_integer_, length(delta))
        mapping_component_covs <- integer()

        if (any(cnm_match)) {
          ll <- lambda[[f]][, names(cnm_match[cnm_match]), drop = FALSE] - 2L
        } else {
          mapping_component[delta != 0] <- -1L
          mapping_component <- lapply(
            mapping_component, function(x) {
              rep(x, each = max(delta))
            }
          )
          if (!is.null(fi)) {
            mapping_component_covs <- mapping_component
          }
          ret <- list(
            mapping_component = mapping_component,
            mapping_component_covs = mapping_component_covs
          )
          return(ret)
        }

        for (j in seq_along(cnm)) {
          cn <- extract_name(factor[[f]], cnm[[j]])

          inds <- which(data[, cn] != 0)

          if (!is.null(fi)) {
            if (Reduce(sum, cnms_match) > 1) {
              stop(
                "Interaction with latent variables currently only ",
                "possible when the loading matrix has a single column."
              )
            }

            mapping_component_covs <- Map(function(x, y) {
              as.numeric(stats::model.matrix(fi[[y]], data = data[x, ]))
            }, x = inds, y = data[inds, load.var])
          }

          inds_expanded <- unlist(Map(function(x, y) {
            rep(x, each = y)
          }, x = inds, y = pmin(1, delta[inds])))

          mapping_component[inds_expanded] <-
            Map(function(x, y) rep(ll[x, cn], each = y),
              x = data[inds, lv], y = delta[inds]
            )
        }

        list(
          mapping_component = mapping_component,
          mapping_component_covs = mapping_component_covs
        )
      })

      lambda_mapping_Zt <- mappingunwrapping(mappings, "mapping_component")

      if (!is.null(fi)) {
        # Extra loadings needed
        extra_lambdas <- extend_lambda(fi)

        # Add indices in the right place in lambda_mapping_Zt
        mlm <- max(lambda_mapping_Zt, na.rm = TRUE)
        lambda_mapping_Zt <- lapply(lambda_mapping_Zt, function(x) {
          c(x, extra_lambdas[[x + 2L]] + mlm)
        })

        lambda_mapping_Zt_covs <- mappingunwrapping(
          mappings, "mapping_component_covs",
          function(...) list(...),
          recursive = FALSE
        )

        # Add indices to lambda matrix
        lambda[[f]] <- rbind(
          lambda[[f]],
          matrix(sort(unique(unlist(extra_lambdas)) + mlm), ncol = 1) + 2L
        )

        # Go through lambda_mapping_Zt_covs and make sure it matches
        # lambda_mapping_Zt
        mplist <- squeeze_mappings(lambda_mapping_Zt, lambda_mapping_Zt_covs)
        lambda_mapping_Zt <- mplist$lambda_mapping_Zt
        lambda_mapping_Zt_covs <- mplist$lambda_mapping_Zt_covs
      } else {
        lambda_mapping_Zt <- lambda_mapping_Zt[!is.na(lambda_mapping_Zt)]
        stopifnot(length(lambda_mapping_Zt) == sum(diff(Zt@p)))
      }
    }
  }

  list(
    lambda_mapping_X = lambda_mapping_X,
    lambda_mapping_Zt = lambda_mapping_Zt,
    lambda_mapping_Zt_covs = lambda_mapping_Zt_covs,
    lambda = lambda
  )
}
