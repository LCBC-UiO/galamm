factor_finder <- function(factor, vars) {
  vapply(factor, function(x) {
    any(vapply(vars, function(y) {
      any(vapply(x, function(z) grepl(z, y), TRUE))
    }, TRUE))
  }, TRUE)
}

define_factor_mappings <- function(
    gobj, load.var, lambda, factor, factor_interactions, data
    ) {
  vars_in_fixed <- all.vars(gobj$fake.formula[-2])

  # Add fixed part of smooth terms
  vars_in_fixed <- c(vars_in_fixed, unlist(lapply(gobj$G$smooth, function(x) {
    if (x$null.space.dim > 0) {
      x$label
    }
  })))
  vars_in_fixed <- unique(vars_in_fixed)
  factor_in_fixed <- factor_finder(factor, vars_in_fixed)
  if (!any(factor_in_fixed)) lambda_mapping_X <- integer()

  X <- gobj$lmod$X
  for (f in seq_along(factor_in_fixed)) {
    if (factor_in_fixed[[f]]) {
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

        ll[data[, load.var]]
      })

      lambda_mapping_X <- do.call(c, mappings)

      stopifnot(length(lambda_mapping_X) == length(X))
    }
  }

  vars_in_random <- unique(unlist(gobj$lmod$reTrms$cnms))
  factor_in_random <- factor_finder(factor, vars_in_random)
  Zt <- gobj$lmod$reTrms$Zt

  if (any(factor_in_random)) {
    lambda_mapping_Zt <- rep(-1L, sum(diff(Zt@p)))
  } else {
    lambda_mapping_Zt <- integer()
  }
  if(!is.null(factor_interactions)) {
    lambda_mapping_Zt_covs <-
      vector(mode = "list", length = length(lambda_mapping_Zt))
  } else {
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
          ret <- list(
            mapping_component = lapply(
              mapping_component, function(x) { rep(x, each = max(delta)) }),
            mapping_component_covs = mapping_component_covs)
          return(ret)
        }

        for (j in seq_along(cnm)) {
          cn <- unlist(lapply(factor[[f]], function(x) {
            m <- regexpr(x, cnm[[j]], fixed = TRUE)
            regmatches(cnm[[j]], m)
          }))

          inds <- which(data[, cn] != 0)

          if(!is.null(fi)) {
            if(j != 1 || i != 1) {
              stop("Interaction with latent variables currently only ",
                   "possible when the loading matrix has a single column.")
            }

            mapping_component_covs <- Map(function(x, y) {
              as.numeric(model.matrix(fi[[y]], data = data[x, ]))
            }, x = inds, y = data[inds, load.var])

          }

          inds_expanded <- unlist(Map(function(x, y) {
            rep(x, each = y)
          }, x = inds, y = pmin(1, delta[inds])))

          mapping_component[inds_expanded] <-
            Map(function(x, y) rep(ll[x, cn], each = y),
              x = data[inds, load.var], y = delta[inds]
            )
        }

        list(mapping_component = mapping_component,
             mapping_component_covs = mapping_component_covs)
      })

      lambda_mapping_Zt <- unlist(do.call(function(...) {
        mapply(c, ..., SIMPLIFY = FALSE)
      }, lapply(mappings, function(x) x$mapping_component)))
      lambda_mapping_Zt <- lambda_mapping_Zt[!is.na(lambda_mapping_Zt)]

      stopifnot(length(lambda_mapping_Zt) == sum(diff(Zt@p)))

      lambda_mapping_Zt_covs <- mappings[[1]]$mapping_component_covs

      if(!is.null(fi)) {
        # Extra loadings needed
        extra_lambdas <- list()
        for(k in seq_along(fi)) {
          if(k == 1) {
            current_max <- 0
          } else {
            inds <- seq(from = 1, to = k - 1, by = 1)
            current_max <- max(vapply(extra_lambdas[inds], function(x) {
              if(length(x) == 0) {
                0
              } else {
                max(x)
              }
            }, numeric(1)))
          }

          extra_lambdas[[k]] <-
            seq_along(attr(terms(fi[[k]]), "term.labels")) +
            current_max
        }

        # Add indices in the right place in lambda_mapping_Zt
        mlm <- max(lambda_mapping_Zt)
        lambda_mapping_Zt <- lapply(lambda_mapping_Zt, function(x) {
          c(x, extra_lambdas[[x + 2L]] + mlm)
        })

        # Add indices to lambda matrix
        lambda[[f]] <- rbind(
          lambda[[f]],
          matrix(sort(unique(unlist(extra_lambdas)) + mlm), ncol = 1) + 2L)

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
