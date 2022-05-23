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

test_that("find_factors works", {
  expect_equal(
    find_factors(lme4::findbars(~ (lambda | item) + (kappa | totem)),
                 data = data.frame(x = 1)),
    list("lambda", "kappa"))
})

test_that("galamm fails when load variable is not in data", {
  expect_error(galamm(
    formula = y ~ 1,
    data = latent_response_dat,
    family = binomial,
    latent = ~ (1 | id_unknown),
    lambda = list(c(1, NA_real_, NA_real_))
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

test_that("find_load_vars works", {
  expect_equal(
    find_load_vars(lme4::findbars(~ (lambda | item) + (kappa | jtem)),
                   data = data.frame(item = 1, jtem = 2),
                   lambda = list(item = 1, jtem = 2)),
    list("item", "jtem"))
})


test_that("initialize_lambda works", {

  set.seed(2)
  ret <- initialize_lambda(list(item1 = c(1, NA, NA),
                                item2 = c(1, 1, NA, NA, NA)))

  expect_equal(names(ret), c("item1", "item2"))
  expect_equal(attr(ret$item1, "free_inds"), c(FALSE, TRUE, TRUE))
  expect_equal(attr(ret$item2, "free_inds"), c(FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(c(ret$item1[[1]], ret$item2[1:2]), c(1, 1, 1))
})

test_that("add_latent_to_data fails with wrong vector size", {
  expect_error(
    galamm(
      formula = y ~ item + latent + time : latent + ( 0 + latent | id ) +
        (0 + latent | tp:id),
      data = latent_response_dat,
      family = binomial,
      latent = ~ (latent | item),
      lambda = list(item = c(1, NA_real_, NA_real_, NA_real_))
    ),
    regexp = "Number of factors does not match number in data.")
})
