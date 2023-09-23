factor_finder <- function(factor, vars) {
  vapply(factor, function(x) {
    any(vapply(vars, function(y) {
      any(vapply(x, function(z) grepl(z, y), TRUE))
    }, TRUE))
  }, TRUE)
}

define_factor_mappings <- function(gobj, load.var, lambda, factor, data) {
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

  for (f in seq_along(factor_in_random)) {
    if (factor_in_random[[f]]) {
      mappings <- lapply(seq_along(gobj$lmod$reTrms$cnms), function(i) {
        delta <- diff(gobj$lmod$reTrms$Ztlist[[i]]@p)
        cnms <- gobj$lmod$reTrms$cnms[[i]]

        mapping_component <- rep(NA_integer_, length(delta))

        cnms_match <- vapply(
          colnames(lambda[[f]]),
          function(x) any(grepl(x, cnms)), TRUE
        )
        if (any(cnms_match)) {
          ll <- lambda[[f]][, names(cnms_match[cnms_match]), drop = FALSE] - 2L
        } else {
          mapping_component[delta != 0] <- -1L
          return(lapply(mapping_component, function(x) {
            rep(x, each = max(delta))
          }))
        }

        for (j in seq_along(cnms)) {
          cn <- unlist(lapply(factor[[f]], function(x) {
            m <- regexpr(x, cnms[[j]], fixed = TRUE)
            regmatches(cnms[[j]], m)
          }))

          inds <- which(data[, cn] != 0)
          inds_expanded <- unlist(Map(function(x, y) {
            rep(x, each = y)
          }, x = inds, y = pmin(1, delta[inds])))

          mapping_component[inds_expanded] <-
            Map(function(x, y) rep(ll[x, cn], each = y),
              x = data[inds, load.var], y = delta[inds]
            )
        }

        mapping_component
      })

      lambda_mapping_Zt <- unlist(do.call(function(...) {
        mapply(c, ..., SIMPLIFY = FALSE)
      }, mappings))
      lambda_mapping_Zt <- lambda_mapping_Zt[!is.na(lambda_mapping_Zt)]

      stopifnot(length(lambda_mapping_Zt) == sum(diff(Zt@p)))
    }
  }

  list(
    lambda_mapping_X = lambda_mapping_X,
    lambda_mapping_Zt = lambda_mapping_Zt
  )
}
