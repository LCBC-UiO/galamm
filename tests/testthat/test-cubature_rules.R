test_that("cubature rules work", {

  dim <- 4
  for(type in c("lu_darmofal_4.1", "lu_darmofal_4.2", "gauss-hermite")){
    rules <- generate_cubature_rules(dimension = dim,
                                     num_quadrature_points = rep(6, dim),
                                     type = type)

    normalizing_constant <- sum(apply(rules$quadrature_points, 1, function(x) 1)
                                * rules$quadrature_weights)

    expect_equal(round(normalizing_constant, 10), 1)

    first_moments <- lapply(seq_len(dim), function(axis){
      sum(apply(rules$quadrature_points, 1, function(x) x[axis]) *
            rules$quadrature_weights)
    })

    expect_equal(round(unlist(first_moments), 10), rep(0, dim))

    second_moments <- lapply(seq_len(dim), function(axis){
      sum(apply(rules$quadrature_points, 1, function(x) x[axis]^2) *
            rules$quadrature_weights)
    })

    expect_equal(round(unlist(second_moments), 10), rep(1, dim))
  }

  # Lu and Darmofal, section 6.2
  for(dim in 4:5){
    rules1 <- generate_cubature_rules(dim, type = "lu_darmofal_4.1")
    rules2 <- generate_cubature_rules(dim, type = "lu_darmofal_4.2")

    integrand <- function(x) (1 + t(x) %*% x)^(-1/2)
    expect_equal(
      sum(apply(rules1$quadrature_points, 1, integrand) * rules1$quadrature_weights),
      sum(apply(rules2$quadrature_points, 1, integrand) * rules2$quadrature_weights))
  }

})
