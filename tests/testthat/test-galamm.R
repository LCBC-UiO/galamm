latent_response_dat <- readRDS(
  system.file("testdata", "latent_response_dat.rds", package = "galamm"))

formula = y ~ item + latent + time : latent + ( 0 + latent | id ) +
  (0 + latent | tp:id)
#data = subset(latent_response_dat, id %in% 1:2 & tp %in% 1:2 & item %in% 1:2)
data <- data[sample(nrow(data), nrow(data)), ]
family = binomial
latent = ~ (latent | item)
lambda = list(item = c(1, NA_real_))


test_that("model matrices get correctly set up", {
  # mod <- galamm(
  #   formula = y ~ item + latent + time : latent + ( 0 + latent | id / tp ),
  #   data = latent_response_dat,
  #   family = binomial,
  #   latent = ~ (latent | item),
  #   lambda = list(item = c(1, NA_real_, NA_real_))
  # )

  # Dummy test while developing
  expect_equal(1, 1)
})
