context("residuals: check if it works")

data(obk.long, package = "afex")
between <- aov_car(
  value ~ treatment * gender + Error(id),
  data = obk.long,
  fun_aggregate = mean
)
mixed <- aov_car(
  value ~ treatment * gender + Error(id / (phase * hour)),
  data = obk.long
)
within <- aov_car(value ~ 1 + Error(id / (phase * hour)), data = obk.long)

## between data with correct order
obk2 <- aggregate(value ~ gender + treatment + id, data = obk.long, FUN = mean)
between2 <- aov_car(value ~ treatment * gender + Error(id), data = obk2)
between2lm <- lm(value ~ treatment * gender, data = obk2)

between3 <- aov_car(
  value ~ treatment * gender + Error(id),
  data = obk2[rev(seq_len(nrow(obk2))), ]
)


## within data with correct order
obk3 <- obk.long[with(obk.long, order(id, phase, hour)), ]
within2 <- aov_car(value ~ 1 + Error(id / (phase * hour)), data = obk3)

test_that("Residuals", {
  expect_message(residuals(within), "Data was changed")
  expect_message(residuals(mixed), "Data was changed")
  expect_message(residuals(between), "Data was changed")
  expect_message(residuals(between3), "Data was changed")

  expect_message(residuals(between2), regexp = NA)
  expect_message(residuals(within2), regexp = NA)

  expect_equal(residuals(between2), residuals(between2lm))

  expect_is(suppressWarnings(residuals(within)), "numeric")
  expect_is(suppressWarnings(residuals(mixed)), "numeric")
  expect_is(suppressWarnings(residuals(between)), "numeric")

  expect_is(residuals(within, append = TRUE), "data.frame")
  expect_is(residuals(mixed, append = TRUE), "data.frame")
  expect_is(residuals(between, append = TRUE), "data.frame")
})


test_that("Fitted", {
  expect_message(fitted(within))
  expect_message(fitted(mixed))
  expect_message(fitted(between))

  expect_message(fitted(between2), regexp = NA)
  expect_message(fitted(within2), regexp = NA)

  expect_equal(fitted(between2), fitted(between2lm))

  expect_is(suppressWarnings(fitted(within)), "numeric")
  expect_is(suppressWarnings(fitted(mixed)), "numeric")
  expect_is(suppressWarnings(fitted(between)), "numeric")

  expect_is(fitted(within, append = TRUE), "data.frame")
  expect_is(fitted(mixed, append = TRUE), "data.frame")
  expect_is(fitted(between, append = TRUE), "data.frame")
})
