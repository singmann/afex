context("afex_plot: basic functionality")

test_that("all input type works", {
  data(obk.long, package = "afex")
  # estimate mixed ANOVA on the full design:
  a1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
                data = obk.long, observed = "gender")
  em1 <- get_emmeans(a1, ~phase*hour, ~treatment+gender)
  em2 <- get_emmeans(a1, c("phase", "hour"), ~treatment+gender)
  em3 <- get_emmeans(a1, ~phase*hour, c("treatment", "gender"))
  em4 <- get_emmeans(a1, c("phase", "hour"), c("treatment", "gender"))
  expect_equal(em1, em2)
  expect_equal(em1, em3)
  expect_equal(em1, em4)
  
  em5 <- get_emmeans(a1, c("phase", "hour"))
  em6 <- get_emmeans(a1, ~phase*hour)
  expect_equal(em5, em6)
})