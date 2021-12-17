test_that("multiplication works", {

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
  expect_equal(value, c(0, 1/2, 0, 3/4, 0))

})
