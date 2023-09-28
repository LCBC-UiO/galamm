setup_factor <- function(load.var, lambda, factor, data) {


  if(!is.null(load.var) && (!is.character(load.var) || length(load.var) != 1)) {
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

release_questions <- function() {
  "Did you re-build the vignettes using `rebuild-long-running-vignette.R`?"
}
