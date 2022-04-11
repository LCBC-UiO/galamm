devtools::load_all()
latent_response_dat <- readRDS(
  system.file("testdata", "latent_response_dat.rds", package = "galamm"))

formula = y ~ item + time : latent + ( 0 + latent | id / tp)
#data = subset(latent_response_dat, id %in% 1:2 & tp %in% 1:2 & item %in% 1:2)
data <- latent_response_dat
data <- data[sample(nrow(data), nrow(data)), ]
family = binomial
latent = ~ (latent | item)
lambda = list(item = c(1, NA_real_, NA_real_))
