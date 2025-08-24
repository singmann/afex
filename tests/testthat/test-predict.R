context("predict")

data(obk.long, package = "afex")
df <- obk.long
df$hour <- as.numeric(df$hour)

newdata <- df[1:5, ]


test_that("between (compare to lm)", {
  between <- aov_car(
    value ~ treatment * gender + Error(id),
    data = df,
    fun_aggregate = mean
  )
  fit.lm <- lm(value ~ treatment * gender, data = between$data$long)

  expect_equal(predict(between, newdata), predict(fit.lm, newdata))
  expect_is(predict(between, newdata), "numeric")
  expect_is(predict(between, newdata, append = TRUE), "data.frame")
})

test_that("within + factorized numerics", {
  within <- aov_car(value ~ 1 + Error(id / (phase * hour)), data = df)

  p1 <- predict(within, newdata = data.frame(phase = "pre", hour = 1))
  p2 <- predict(within, newdata = data.frame(phase = "pre", hour = "1"))
  expect_equal(p1, p2)
  expect_equal(unname(p1), 3.8125)
  expect_error(predict(within, newdata = data.frame(phase = "pre", hour = 6)))
})
