
context("ANOVAs: structural tests")

test_that("dv is numeric", {
  data(obk.long)
  expect_that(aov_car(treatment ~ gender + Error(id/phase*hour), data = obk.long, observed = "gender"), throws_error("dv needs to be numeric."))
})

test_that("non Type 3 sums give warning", {
  data(obk.long)
  expect_that(aov_4(value ~ treatment * gender + (phase*hour|id), data = obk.long, observed = "gender", check.contrasts = FALSE), gives_warning("contrasts"))
})

test_that("return='aov' works", {
  data(obk.long)
  data(md_12.1)
  
  # purely within
  expect_that(aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), return = "aov"), is_a(c( "aovlist", "listof" )))
  expect_that(aov_car(value ~ Error(id/phase*hour), data = obk.long, return = "aov"), is_a(c( "aovlist", "listof" )))
  #purely between
  expect_that(suppressWarnings(aov_car(value ~ treatment * gender + Error(id), data = obk.long, return = "aov")), is_a(c( "aov")))
  expect_that(suppressWarnings(aov_car(value~treatment * gender + Error(id/phase*hour), data = obk.long, return = "aov")), is_a(c( "aovlist", "listof" )))
  
  # terms within Error() are within parentheses:
  test <- summary(aov_car(value ~ Error(id/phase*hour), data = obk.long, return = "aov"))
  positive  <- summary(aov(value ~ phase*hour+Error(id/(phase*hour)), data = obk.long))
  negative  <- summary(aov(value ~ phase*hour+Error(id/phase*hour), data = obk.long))
  expect_equal(test, positive)
  expect_false(isTRUE(all.equal(test, negative, check.attributes = FALSE)))
  
  orig1 <- aov_car(value ~ Error(id/phase*hour), data = obk.long)
  obk.long$id <- as.numeric(obk.long$id)
  obk.long$phase <- as.numeric(obk.long$phase)
  obk.long$hour <- as.numeric(obk.long$hour)
  positive2  <- summary(aov_car(value ~ Error(id/phase*hour), data = obk.long, return = "aov"))
  expect_equal(test, positive2)
  positive3 <- aov_car(value ~ Error(id/phase*hour), data = obk.long)
  expect_equal(summary(orig1), summary(positive3))
  expect_equal(summary(orig1$Anova, multivariate = FALSE), summary(positive3$Anova, multivariate = FALSE))
  expect_equal(summary(orig1$aov), summary(positive3$aov))
})
