latent_response_dat <- readRDS(
  system.file("testdata", "latent_response_dat.rds", package = "galamm"))

test_that("galamm fails when loading is a data column", {
  expect_error(galamm(
    formula = y ~ 1,
    data = latent_response_dat,
    family = binomial,
    latent = ~ (time | item)
  ))
})

#
# test_that("model matrices get correctly set up", {
#   mod <- galamm(
#     formula = y ~ item + time : latent + ( 0 + latent | id / tp ),
#     data = latent_response_dat,
#     family = binomial,
#     latent = ~ (latent | item),
#     lambda = c(1, NA_real_, NA_real_)
#   )
# })
