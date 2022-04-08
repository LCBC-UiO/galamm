test_that("update_fixed works", {
  X <- matrix(1:12, nrow = 3)
  expect_equal(
    update_fixed(X, c(0L, 2L), c(.5, 1, 2)),
    structure(c(2, 2, 1.5, 4, 5, 6, 14, 8, 4.5, 10, 11, 12), .Dim = 3:4)
  )
})
