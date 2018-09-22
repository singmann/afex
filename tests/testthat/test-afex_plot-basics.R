context("afex_plot: basic functionality")

test_that("all input type works and warnings ae correct", {
  data(obk.long, package = "afex")
  # estimate mixed ANOVA on the full design:
  a1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
                data = obk.long, observed = "gender")
  
  expect_warning(
    em1 <- afex_plot(a1, ~phase*hour, ~treatment+gender, return = "data"),
    "mixed within-between-design"
  )
  expect_warning(
    em2 <- afex_plot(a1, c("phase", "hour"), ~treatment+gender, 
                     return = "data"),
    "mixed within-between-design"
  )
  expect_warning(
    em3 <- afex_plot(a1, ~phase*hour, c("treatment", "gender"), 
                     return = "data"),
    "mixed within-between-design"
  )
  expect_warning(
    em4 <- afex_plot(a1, c("phase", "hour"), c("treatment", "gender"), 
                     return = "data"),
    "mixed within-between-design"
  )
  
  expect_equal(em1, em2)
  expect_equal(em1, em3)
  expect_equal(em1, em4)
  
  expect_warning(
    em5 <- afex_plot(a1, c("phase", "hour"), return = "data"),
    "show within-subjects factors, but not within-subjects error bars"
  )
  expect_warning(
    em6 <- afex_plot(a1, ~phase*hour, return = "data"),
    "show within-subjects factors, but not within-subjects error bars"
  )
  expect_equal(em5, em6)
  
  expect_warning(
    em7 <- afex_plot(a1, c("treatment", "gender"), panel = "phase", 
                     return = "data", error = "within"),
    "between-subjects factors, but within-subjects error bars"
  )
  expect_warning(
    em8 <- afex_plot(a1, ~treatment*gender, panel = "phase",
                     return = "data", error = "within"),
    "between-subjects factors, but within-subjects error bars"
  )
  expect_equal(em7, em8)
})
