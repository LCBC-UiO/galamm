test_that("Residuals are correctly computed", {
  gm1 <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     data = lme4::cbpp, family = binomial)

  gm2 <- galamm(cbind(incidence, size - incidence) ~ period + (1 | herd),
                data = lme4::cbpp, family = binomial)

  expect_equal(
    as.numeric(residuals(gm1, type = "pearson")),
    as.numeric(residuals(gm2, type = "pearson")),
    tolerance = .01
  )

})
