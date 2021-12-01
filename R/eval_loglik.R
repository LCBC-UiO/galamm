eval_loglik <- function(pars, moments, Quadpoints,
                        LLHelper, Lambdat,
                        Lind, X, Zt, y, family, maxit = 1,
                        fixed_ind, vc_ind){

  beta <- pars[fixed_ind]
  theta <- exp(pars[vc_ind])
  # Update relative covariance factor after having updated theta
  Lambdat@x[] <- theta[Lind]

  # "raw" locations
  a <- as.matrix(do.call(expand.grid, lapply(Quadpoints, function(x) x$x)))
  # "raw" weights
  p <- as.matrix(do.call(expand.grid, lapply(Quadpoints, function(x) x$w)))

  mainvars <- grep("\\:", colnames(moments[[1]]), invert = TRUE, value = TRUE)
  squaredvars <- paste0(mainvars, ":", mainvars)

  crossedvars <- define_crossedvars(mainvars)

  ll0 <- -Inf
  for(i in seq_len(maxit)){
    mu <- moments[[1]][, mainvars, drop = FALSE]
    tau <- sqrt(moments[[1]][, squaredvars, drop = FALSE] -
                  moments[[1]][, mainvars, drop = FALSE]^2)

    gamma <- (
      moments[[1]][, crossedvars[, "var1var2"], drop = FALSE] -
        moments[[1]][, crossedvars[, "var1"], drop = FALSE] * moments[[1]][, crossedvars[, "var2"], drop = FALSE]
    ) / moments[[1]][, crossedvars[, "var1var1"]]

    A <- P <- alpha <- array(dim = c(ncol(a), nrow(mu), nrow(a)))

    for(iter in seq_len(ncol(a))){
      alpha[iter, ,] <- outer(mu[, iter], rep(1, nrow(a))) +
        outer(tau[, iter], a[, iter])

      gamma_inds <- which(crossedvars[, "var2"] %in% mainvars[[iter]])
      transformation <- alpha[iter , ,] * 0
      for(gi in gamma_inds){
        transformation <- transformation + alpha[gi , ,] * as.numeric(gamma[, gi])
      }
      A[iter, ,] <- alpha[ iter, ,] - transformation
      P[iter, ,] <- sqrt(2 * pi) *
        outer(tau[, iter], exp(a[ , iter]^2 / 2) * p[ , iter]) * stats::dnorm(A[iter, ,])

    }

    # Every column of A is a spherical random effect where we
    # want to evaluate the response probability
    # Create non-spherical random effects
    A_mat <- A
    dim(A_mat) <- c(prod(dim(A)[1:2]), dim(A)[[3]])
    b <- Matrix::crossprod(Lambdat, A_mat)

    # Evaluate linear predictor at each quadrature point
    nu <- Matrix::crossprod(Zt, b) + c(X %*% beta)

    # Evaluate response probability at each quadrature point
    probmat <- log_response_prob(linkinv(nu), y)

    # Sum of level-1 log probabilities per level-2 unit
    f1_prod <- LLHelper %*% probmat

    # Integrate to get level-2 likelihood
    P_prod <- eval(parse(text = paste0("P[", seq_len(dim(P)[[1]]), ", , ]", collapse = "*")))
    f2 <- log(Matrix::rowSums(exp(f1_prod) * P_prod))

    for(varind in seq_along(mainvars)){
      moments[[1]][, mainvars[[varind]]] <-
        Matrix::rowSums(exp(f1_prod) * P_prod * A[varind, , ]) / exp(f2)
    }
    for(varind in seq_along(squaredvars)){
      moments[[1]][, squaredvars[[varind]]] <-
        Matrix::rowSums(exp(f1_prod) * P_prod * A[varind, , ]^2) / exp(f2)
    }
    for(varind in seq_along(crossedvars[, "var1var2"])){
      ind1 <- which(mainvars == crossedvars[varind, "var1"])
      ind2 <- which(mainvars == crossedvars[varind, "var2"])
      v1v2 <- crossedvars[varind, "var1var2"]
      moments[[1]][, v1v2] <-
        Matrix::rowSums(exp(f1_prod) * P_prod * A[ind1, , ] * A[ind2, , ]) / exp(f2)
      }

    ll <- sum(f2)
    if(abs(ll0 - ll) < 1e-3) break
    ll0 <- ll
  }
  attr(ll, "moments") <- moments
  attr(ll, "iter") <- i
  ll
}
