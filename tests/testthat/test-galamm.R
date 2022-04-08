latent_response_dat <- readRDS(
  system.file("testdata", "latent_response_dat.rds", package = "galamm"))

test_that("galamm fails when factor is a data column", {
  expect_error(galamm(
    formula = y ~ 1,
    data = latent_response_dat,
    family = binomial,
    latent = ~ (time | item)
  ),
  regexp = "Factors in argument 'latent' cannot be columns of 'data'.")
})

test_that("galamm fails when load variable is not in data", {
  expect_error(galamm(
    formula = y ~ 1,
    data = latent_response_dat,
    family = binomial,
    latent = ~ (1 | id_unknown)
  ),
  regexp = "All loading variables in 'latent' must be columns of 'data'.")
})

test_that("galamm fails when load variable is not in lambda", {
  expect_error(galamm(
    formula = y ~ 1,
    data = latent_response_dat,
    family = binomial,
    latent = ~ (1 | item),
    lambda = list(other_item = c(1, NA, NA))
  ),
  regexp = "All loading variables specified in 'latent' must be in 'lambda'.")
})


test_that("model matrices get correctly set up", {
  mod <- galamm(
    formula = y ~ item + latent + time : latent + ( 0 + latent | id / tp ),
    data = latent_response_dat,
    family = binomial,
    latent = ~ (latent | item),
    lambda = list(item = c(1, NA_real_, NA_real_))
  )
})
