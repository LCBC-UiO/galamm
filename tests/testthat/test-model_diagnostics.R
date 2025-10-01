test_that("Residuals are correctly computed with binomial outcome", {
  gm1 <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     data = lme4::cbpp, family = binomial)

  gm2 <- galamm(cbind(incidence, size - incidence) ~ period + (1 | herd),
                data = lme4::cbpp, family = binomial)

  expect_equal(
    as.numeric(residuals(gm1, type = "pearson")),
    as.numeric(residuals(gm2, type = "pearson")),
    tolerance = .01
  )

  expect_equal(
    as.numeric(residuals(gm1, type = "deviance")),
    as.numeric(residuals(gm2, type = "deviance")),
    tolerance = .01
  )
})

test_that("Residuals are correctly computed with Poisson outcome", {
  gm1 <- lme4::glmer(
    formula = y ~ lbas * treat + lage + v4 + (1 | subj),
    data = epilep,
    family = poisson
  )

  gm2 <- galamm(
    formula = y ~ lbas * treat + lage + v4 + (1 | subj),
    data = epilep,
    family = poisson
  )

  expect_equal(
    as.numeric(residuals(gm1, type = "pearson")),
    as.numeric(residuals(gm2, type = "pearson")),
    tolerance = .01
  )

  expect_equal(
    as.numeric(residuals(gm1, type = "deviance")),
    as.numeric(residuals(gm2, type = "deviance")),
    tolerance = .01
  )
})
