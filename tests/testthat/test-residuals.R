context("residuals: check if it works")

data(obk.long, package = "afex")
between <- aov_car(value ~ treatment*gender + Error(id), data = obk.long)
mixed <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)
within <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk.long)

test_that("Residuals works", {
  expect_is(residuals(within),"numeric")
  expect_is(residuals(mixed),"numeric")
  expect_is(residuals(between),"numeric")
  
  expect_is(residuals(within, append = TRUE),"data.frame")
  expect_is(residuals(mixed, append = TRUE),"data.frame")
  expect_is(residuals(between, append = TRUE),"data.frame")
})
