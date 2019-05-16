context("residuals: check if it works")

data(obk.long, package = "afex")
between <- aov_car(value ~ treatment*gender + Error(id), data = obk.long)
mixed <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)
within <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk.long)

test_that("Residuals works", {
  # different output for different types of residuals
  expect_is(residuals(within, model = "multivariate"),"matrix")
  expect_is(residuals(mixed, model = "multivariate"),"matrix")
  
  expect_is(residuals(within, model = "univariate"),"list")
  expect_is(residuals(mixed, model = "univariate"),"list")
  
  expect_is(residuals(between, model = "univariate"),"numeric")
  
  # multivariate and univariate residuals are the same for between-ANOVA only
  expect_equal(residuals(between, model = "multivariate"),
               residuals(between, model = "univariate"))
  
  # plot
  expect_is(residuals_qqplot(within),"ggplot")
  expect_is(residuals_qqplot(mixed),"ggplot")
  expect_is(residuals_qqplot(between),"ggplot")
})
