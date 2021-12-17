test_that("spherical cubature rules work", {

  # mean is always exactly zero
  value <- vapply(2:7, function(dim){
    rules <- generate_cubature_rules(dim)
    sum(vapply(seq_len(rules$num_quadrature_points), function(i){
      mean(rules$quadrature_points[i, ]) * rules$quadrature_weights[i]
    }, FUN.VALUE = numeric(1)))
  }, FUN.VALUE = numeric(1))
  expect_equal(value, rep(0, length(2:7)))

  # moments along the first axis
  dim <- 5
  rules <- generate_cubature_rules(dim)
  value <- vapply(1:5, function(pow){
    sum(vapply(seq_len(rules$num_quadrature_points), function(i){
      rules$quadrature_points[i, 1]^(pow) *
        rules$quadrature_weights[i]
    }, FUN.VALUE = numeric(1))) /
      sum(vapply(seq_len(rules$num_quadrature_points), function(i){
        rules$quadrature_weights[i]
      }, FUN.VALUE = numeric(1)))
  }, FUN.VALUE = numeric(1))
  expect_equal(value, c(0, 1, 0, 3, 0))

})

test_that("cartesian cubature rules work", {
  # mean is always exactly zero
  value <- vapply(2:5, function(dim){
    rules <- generate_cubature_rules(dim, num_quadrature_points = rep(4, dim),
                                     type = "gauss-hermite")
    sum(vapply(seq_len(rules$num_quadrature_points), function(i){
      mean(rules$quadrature_points[i, ]) * rules$quadrature_weights[i]
    }, FUN.VALUE = numeric(1)))
  }, FUN.VALUE = numeric(1))
  expect_equal(round(value, 10), rep(0, length(2:5)))

  # moments along the first axis
  dim <- 5
  rules <- generate_cubature_rules(
    dim, num_quadrature_points = rep(3, dim),
    type = "gauss-hermite")
  value <- vapply(1:5, function(pow){
    sum(vapply(seq_len(rules$num_quadrature_points), function(i){
      rules$quadrature_points[i, 1]^(pow) *
        rules$quadrature_weights[i]
    }, FUN.VALUE = numeric(1))) /
      sum(vapply(seq_len(rules$num_quadrature_points), function(i){
        rules$quadrature_weights[i]
      }, FUN.VALUE = numeric(1)))
  }, FUN.VALUE = numeric(1))
  expect_equal(round(value, 10), c(0, 1, 0, 3, 0))
})


test_that("replicate quadrature works", {

  rules <- generate_cubature_rules(3)
  reprules <- replicate_quadrature_points(rules, 2)
  expect_equal(reprules$num_quadrature_points,
               rules$num_quadrature_points)

  expect_equal(
    reprules$quadrature_points,
    rbind(t(rules$quadrature_points), t(rules$quadrature_points))
  )
  expect_equal(
    reprules$quadrature_weights,
    rbind(rules$quadrature_weights, rules$quadrature_weights)
  )
})
