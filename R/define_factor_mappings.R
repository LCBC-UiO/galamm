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

  for (f in seq_along(factor_in_random)) {
    cnms <- lapply(gobj$lmod$reTrms$cnms, function(x) x)
    cnms_match <- lapply(cnms, function(cnm) {
      vapply(
        colnames(lambda[[f]]),
        function(x) any(grepl(x, cnm)), TRUE
        )
    })

    if (factor_in_random[[f]]) {
      mappings <- lapply(seq_along(cnms), function(i) {
        cnm <- cnms[[i]]
        cnm_match <- cnms_match[[i]]
        delta <- diff(gobj$lmod$reTrms$Ztlist[[i]]@p)
        mapping_component <- rep(NA_integer_, length(delta))

        if (any(cnm_match)) {
          ll <- lambda[[f]][, names(cnm_match[cnm_match]), drop = FALSE] - 2L
        } else {
          mapping_component[delta != 0] <- -1L
          return(lapply(mapping_component, function(x) {
            rep(x, each = max(delta))
          }))
        }

        for (j in seq_along(cnm)) {
          cn <- unlist(lapply(factor[[f]], function(x) {
            m <- regexpr(x, cnm[[j]], fixed = TRUE)
            regmatches(cnm[[j]], m)
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

      fi <- factor_interactions[[f]]
      if(!is.null(fi)) {

        mlm <- max(lambda_mapping_Zt)
        lambda_mapping_Zt <- lapply(lambda_mapping_Zt, function(x) {
          current_formula <- fi[[x + 2L]]
          tt <- terms(current_formula)
          if(length(attr(tt, "factors")) == 0) {
            x
          } else {
            c(x, seq(from = mlm + 1, length.out = ncol(attr(tt, "factors"))))
          }
        })
        mlm <- max(vapply(lambda_mapping_Zt, max, numeric(1))) + 2L

        # Add rows to lambda matrix
        if(ncol(lambda[[f]]) > 1) {
          stop("Currently latent covariates are only supported with ",
               "lambda matrices having a single column.")
        }

        lambda[[f]] <- rbind(lambda[[f]],
                             matrix(seq(from = max(lambda[[f]] + 1),
                                        to = mlm, by = 1), ncol = 1))

        lambda_mapping_Zt_covs <-
          vector(mode = "list", length = length(lambda_mapping_Zt))


      }
    }
  }

  list(
    lambda_mapping_X = lambda_mapping_X,
    lambda_mapping_Zt = lambda_mapping_Zt
  )
}
