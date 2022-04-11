

test_that("update_fixed works", {
  X <- matrix(1:12, nrow = 3)
  expect_equal(
    update_fixed(X, c(0L, 2L), c(.5, 1, 2)),
    structure(c(2, 2, 1.5, 4, 5, 6, 14, 8, 4.5, 10, 11, 12), .Dim = 3:4)
  )
})

test_that("update_random works", {
  mm <- matrix(
    c(1, 0, 0,
      2, 0, 0,
      0, 1, 0,
      0, 2, 0,
      0, 0, 3), ncol = 3, byrow = TRUE)
  Zt <- as(mm, Class = "sparseMatrix")
  lambda_update <- Zt@x

  Ztu <- update_random(Zt, lambda_update)
  expect_equal(Ztu@x, rep(1, 5))
})
