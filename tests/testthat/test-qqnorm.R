context("residuals: qqnorm plot")

data(obk.long, package = "afex")
between <- aov_car(value ~ treatment*gender + Error(id), data = obk.long, 
                   fun_aggregate = mean)
mixed <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)
within <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk.long)

test_that("return plot", {
  expect_is(qqnorm(between), "gg")
  expect_is(qqnorm(mixed), "gg")
  expect_is(qqnorm(within), "gg")
})

test_that("return fd", {
  expect_is(qqnorm(between, return = "data"), "data.frame")
  expect_is(qqnorm(mixed, return = "data"), "data.frame")
  expect_is(qqnorm(within, return = "data"), "data.frame")
})
