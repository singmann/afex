context("residuals: check if it works")

data(obk.long, package = "afex")
between <- aov_car(value ~ treatment*gender + Error(id), data = obk.long)
mixed <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)
within <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk.long)

obk2 <- aggregate(value ~ gender + treatment + id , data = obk.long, FUN = mean)
between2 <- aov_car(value ~ treatment*gender + Error(id), data = obk2)
between2lm <- lm(value ~ treatment*gender, data = obk2)


test_that("Residuals", {
  expect_warning(residuals(within))
  expect_warning(residuals(mixed))
  expect_warning(residuals(between))
  expect_warning(residuals(between2), regexp = NA)
  
  expect_equal(residuals(between2), unname(residuals(between2lm)))
  
  expect_is(suppressWarnings(residuals(within)),"numeric")
  expect_is(suppressWarnings(residuals(mixed)),"numeric")
  expect_is(suppressWarnings(residuals(between)),"numeric")
  
  expect_is(residuals(within, append = TRUE),"data.frame")
  expect_is(residuals(mixed, append = TRUE),"data.frame")
  expect_is(residuals(between, append = TRUE),"data.frame")
})


test_that("Fitted", {
  expect_warning(fitted(within))
  expect_warning(fitted(mixed))
  expect_warning(fitted(between))
  expect_warning(fitted(between2), regexp = NA)
  
  expect_equal(fitted(between2), unname(fitted(between2lm)))
  
  expect_is(suppressWarnings(fitted(within)),"numeric")
  expect_is(suppressWarnings(fitted(mixed)),"numeric")
  expect_is(suppressWarnings(fitted(between)),"numeric")
  
  expect_is(fitted(within, append = TRUE),"data.frame")
  expect_is(fitted(mixed, append = TRUE),"data.frame")
  expect_is(fitted(between, append = TRUE),"data.frame")
})
