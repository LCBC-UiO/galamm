## code to prepare `hermite_quadrature` dataset goes here
library(gaussquad)
Quadpoints <- hermite.h.quadrature.rules(100)

hermite_quadrature <- lapply(Quadpoints, function(qp){
  sweep(qp, 2, c(sqrt(2), 1 / sqrt(pi)), "*")
})

usethis::use_data(hermite_quadrature, overwrite = TRUE, internal = TRUE)
