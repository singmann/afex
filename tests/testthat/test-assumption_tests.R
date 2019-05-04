

context("test_assumptions: check if it works")

data(obk.long, package = "afex")
between_1 <- aov_car(value ~ treatment + Error(id), data = obk.long)
between_2 <- aov_car(value ~ treatment*gender + Error(id), data = obk.long)
mixed <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)
within <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk.long)

test_that("Levene Test works", {
  l1 <- test_levene(between_1)
  l2 <- test_levene(between_2)
  l3 <- test_levene(mixed)
  expect_is(l1, "anova")
  expect_is(l1, "data.frame")
  
  expect_is(l2, "anova")
  expect_is(l2, "data.frame")
  
  expect_is(l3, "anova")
  expect_is(l3, "data.frame")
  
  expect_error(test_levene(within), "between-subjects")
})

test_that("Sphericity Test works", {
  expect_error(test_sphericity(between_1), "within-subjects")
  expect_error(test_sphericity(between_2), "within-subjects")
  
  s1 <- test_sphericity(mixed)
  expect_is(s1, "anova")
  
  s2 <- test_sphericity(within)
  expect_is(s2, "anova")
})

