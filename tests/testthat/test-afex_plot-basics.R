context("afex_plot: basic functionality")

test_that("all input type works", {
  data(obk.long, package = "afex")
  # estimate mixed ANOVA on the full design:
  a1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
                data = obk.long, observed = "gender")
  em1 <- afex_plot(a1, ~phase*hour, ~treatment+gender, return = "data")
  em2 <- afex_plot(a1, c("phase", "hour"), ~treatment+gender, return = "data")
  em3 <- afex_plot(a1, ~phase*hour, c("treatment", "gender"), return = "data")
  em4 <- afex_plot(a1, c("phase", "hour"), c("treatment", "gender"), return = "data")
  expect_equal(em1, em2)
  expect_equal(em1, em3)
  expect_equal(em1, em4)
  
  # em5 <- afex_plot(a1, c("phase", "hour"), return = "data")
  # em6 <- afex_plot(a1, ~phase*hour, return = "data")
  # expect_equal(em5, em6)
})