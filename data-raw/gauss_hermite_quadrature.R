## code to prepare `DATASET` dataset goes here

library(gaussquad)
gauss_hermite_quadrature_rules <- hermite.h.quadrature.rules(100)
usethis::use_data(gauss_hermite_quadrature_rules, overwrite = TRUE,
                  internal = TRUE)
