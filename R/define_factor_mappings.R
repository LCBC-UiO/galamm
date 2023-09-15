define_factor_mappings <- function(gobj, load.var, lambda, factor, data) {
  vars_in_fixed <- all.vars(gobj$fake.formula[-2])
  factor_in_fixed <-
    vapply(factor, function(x) any(x %in% vars_in_fixed), TRUE)
  vars_in_random <- unique(unlist(gobj$lmod$reTrms$cnms))
  factor_in_random <-
    vapply(factor, function(x) any(vapply(vars_in_random, function(y) any(vapply(x, function(z) grepl(z, y), TRUE)), TRUE)), TRUE)

  X <- gobj$lmod$X
  if (any(factor_in_fixed)) {
    lambda_mapping_X <- rep(-1L, length(X))
  } else {
    lambda_mapping_X <- integer()
  }

  for (f in seq_along(factor_in_fixed)) {
    if (factor_in_fixed[[f]]) {
      cols <- unlist(lapply(factor[[1]], function(fact) grep(fact, colnames(X))))
      for (cc in cols) {
        lambda_mapping_X[
          seq(from = (cc - 1) * nrow(X) + 1, to = cc * nrow(X))
        ] <-
          lambda[[1]][data[, load.var]] - 2L
      }
    }
  }

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
          return(lapply(mapping_component, function(x) rep(x, each = max(delta))))
        }


        for (j in seq_along(cnms)) {
          cn <- unlist(lapply(factor[[f]], function(x) {
            m <- regexpr(x, cnms[[j]], fixed = TRUE)
            regmatches(cnms[[j]], m)
          }))

          inds <- which(data[, cn] != 0)
          inds_expanded <- unlist(Map(function(x, y) rep(x, each = y), x = inds, y = pmin(1, delta[inds])))

          mapping_component[inds_expanded] <-
            Map(function(x, y) rep(ll[x, cn], each = y),
              x = data[inds, load.var], y = delta[inds]
            )
        }

        mapping_component
      })

      lambda_mapping_Zt <- unlist(do.call(function(...) mapply(c, ..., SIMPLIFY = FALSE), mappings))
      lambda_mapping_Zt <- lambda_mapping_Zt[!is.na(lambda_mapping_Zt)]

      stopifnot(length(lambda_mapping_Zt) == sum(diff(Zt@p)))
    }
  }

  list(
    lambda_mapping_X = lambda_mapping_X,
    lambda_mapping_Zt = lambda_mapping_Zt
  )
}
