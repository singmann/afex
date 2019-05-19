context("residuals: check if it works")

data(obk.long, package = "afex")
between <- aov_car(value ~ treatment*gender + Error(id), data = obk.long)
mixed <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)
within <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk.long)

test_that("Residuals works", {
  expect_is(residuals(within),"numeric")
  expect_is(residuals(mixed),"numeric")
  expect_is(residuals(between),"numeric")
  
  expect_is(residuals(within, return_df = TRUE),"data.frame")
  expect_is(residuals(mixed, return_df = TRUE),"data.frame")
  expect_is(residuals(between, return_df = TRUE),"data.frame")
  
  # plot
  expect_is(residuals_qqplot(within),"ggplot")
  expect_is(residuals_qqplot(mixed),"ggplot")
  expect_is(residuals_qqplot(between),"ggplot")
})
