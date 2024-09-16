test_that("skip_extended works", {
  current_value <- Sys.getenv("GALAMM_EXTENDED_TESTS")
  Sys.setenv(GALAMM_EXTENDED_TESTS = "true")
  expect_condition(skip_extended(), NA, class = "skip")
  Sys.setenv(GALAMM_EXTENDED_TESTS = "")
  expect_condition(skip_extended(), class = "skip")
  Sys.setenv(GALAMM_EXTENDED_TESTS = current_value)
})
