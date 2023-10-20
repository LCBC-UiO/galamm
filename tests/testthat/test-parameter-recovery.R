#' @srrstats {5.6} Parameter recovery tests are done here.
#' @srrstats {G5.6a} We are testing recovery within a relatively wide tolerance
#'   here.
#' @srrstats {G5.6b} Parameter recovery with multiple random seeds tested here.
#' @srrstats {G5.7} Tests here show that parameter recovery becomes more
#'   accurate as the data size increases.
#' @noRd
NULL

lmm_simulator_function <- function(n_ids, repetition) {
  dat <- merge(
    data.frame(
      id = seq_len(n_ids),
      random_intercept = rnorm(n_ids)
    ),
    expand.grid(
      id = seq_len(n_ids),
      timepoint = 1:3
    ),
    by = "id"
  )
  dat$x <- runif(nrow(dat))
  dat$y <- dat$x + dat$random_intercept + rnorm(nrow(dat), sd = 1)

  mod <- galamm(formula = y ~ x + (1 | id), data = dat)
  vc <- as.data.frame(VarCorr(mod))

  data.frame(
    n_ids = n_ids,
    beta_estimate = coef(mod)[["x"]],
    theta_estimate = vc[1, "sdcor"]
  )
}

test_that("LMM parameters are within a tolerance of their generated value", {
  test <- lapply(1:10, function(i) {
    set.seed(i)
    res <- lmm_simulator_function(n_ids = 100, repetition = 1)
    expect_gt(res$beta_estimate, 0)
    expect_lt(res$beta_estimate, 2)
    expect_gt(res$theta_estimate, 0)
    expect_lt(res$theta_estimate, 2)
  })
})


test_that(
  "LMM parameters are recovered with increasing precision",
  {
    # skip_extended()

    # Simulate data with repeated measurements
    set.seed(10)
    sim_params <- expand.grid(
      n_ids = c(100, 200, 400),
      repetition = 1:10
    )

    simres_list <- Map(lmm_simulator_function,
      n_ids = sim_params$n_ids,
      repetition = sim_params$repetition
    )

    simres <- do.call(rbind, simres_list)
    # True value is 1 for both beta and random intercept standard deviation
    simres_rmse <- aggregate(
      x = cbind(beta_estimate, theta_estimate) ~ n_ids,
      data = simres,
      FUN = function(x) mean((x - 1)^2)
    )

    simres_rmse <- simres_rmse[order(simres_rmse$n_ids), , drop = FALSE]

    # Expect decreasing RMSE with increasing sample size
    expect_true(all(diff(simres_rmse$beta_estimate) < 0))
    expect_true(all(diff(simres_rmse$theta_estimate) < 0))
  }
)

lmm_factor_simulator_function <- function(n_ids, repetition) {
  dat <- merge(
    data.frame(
      id = seq_len(n_ids),
      random_intercept = rnorm(n_ids)
    ),
    expand.grid(
      id = seq_len(n_ids),
      timepoint = 1:3
    ),
    by = "id"
  )
  lambda_true <- c(1, .8, 1.2)
  dat$x <- runif(nrow(dat))
  dat$y <- dat$x + lambda_true[dat$timepoint] * dat$random_intercept +
    rnorm(nrow(dat), sd = 1)

  mod <- galamm(
    formula = y ~ x + (0 + loading | id), data = dat,
    load.var = "timepoint",
    lambda = matrix(c(1, NA, NA), ncol = 1),
    factor = "loading"
  )
  vc <- as.data.frame(VarCorr(mod))
  fl <- factor_loadings(mod)[, 1]

  data.frame(
    n_ids = n_ids,
    beta_estimate = coef(mod)[["x"]],
    theta_estimate = vc[1, "sdcor"],
    lambda2_estimate = fl[[2]],
    lambda3_estimate = fl[[3]]
  )
}

test_that("LMM with factor structure parameter recovery", {
  skip_extended()
  test <- lapply(1:10, function(i) {
    set.seed(i)
    res <- lmm_factor_simulator_function(n_ids = 1000, repetition = 1)
    expect_gt(res$beta_estimate, 0)
    expect_lt(res$beta_estimate, 2)
    expect_gt(res$theta_estimate, 0)
    expect_lt(res$theta_estimate, 2)
    expect_lt(res$lambda2_estimate, res$lambda3_estimate)
    expect_lt(res$lambda2_estimate, 2)
    expect_gt(res$lambda2_estimate, .2)
  })
})

test_that(
  "LMM with factor structure increasing precision",
  {
    skip_extended()

    # Simulate data with repeated measurements
    set.seed(10)
    sim_params <- expand.grid(
      n_ids = c(100, 200, 400),
      repetition = 1:10
    )

    simres_list <- Map(lmm_factor_simulator_function,
      n_ids = sim_params$n_ids,
      repetition = sim_params$repetition
    )

    simres <- do.call(rbind, simres_list)
    # True value is 1 for both beta and random intercept standard deviation
    simres_rmse <- aggregate(
      x = cbind(
        beta = beta_estimate - 1, theta = theta_estimate - 1,
        lambda2 = lambda2_estimate - .8,
        lambda3 = lambda3_estimate - 1.2
      ) ~ n_ids,
      data = simres,
      FUN = function(x) mean(x^2)
    )

    simres_rmse <- simres_rmse[order(simres_rmse$n_ids), , drop = FALSE]

    # Expect decreasing RMSE with increasing sample size
    expect_true(all(diff(simres_rmse$beta) < 0))
    expect_true(all(diff(simres_rmse$theta) < 0))
    expect_true(all(diff(simres_rmse$lambda2) < 0))
    expect_true(all(diff(simres_rmse$lambda3) < 0))
  }
)
