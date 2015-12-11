
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

test_that("anova_table attributes", {
  data(md_12.1)
  no_attr <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), anova_table = list(correction = "none"))
  
  expect_that(attr(no_attr$anova_table, "correction"), equals("none"))
  expect_that(attr(no_attr$anova_table, "p.adjust.method"), equals("none"))
  expect_output(attr(no_attr$anova_table, "observed"), "character\\(0\\)")
  
  all_attr <- suppressWarnings(aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), observed = "angle", anova_table=list(correction = "HF", p.adjust.method = "bonferroni")))
  
  expect_that(attr(all_attr$anova_table, "correction"), equals("HF"))
  expect_that(attr(all_attr$anova_table, "p.adjust.method"), equals("bonferroni"))
  expect_that(attr(all_attr$anova_table, "observed"), equals("angle"))
  expect_output(all_attr, "bonferroni")
  expect_output(all_attr, "HF")
  
  added_attr <- suppressWarnings(nice(no_attr, correction = "HF", p.adjust = "bonferroni", observed = "angle"))
  expect_that(nice(all_attr$anova_table), is_identical_to(added_attr))
  
  reset_attr <- nice(no_attr, correction = "none", p.adjust = "none", observed = NULL)
  expect_that(nice(no_attr$anova_table), is_identical_to(reset_attr))
  
  # Test support for old afex objects
  old_afex_object <- default_options <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))
  attr(old_afex_object$anova_table, "observed") <- NULL
  attr(old_afex_object$anova_table, "correction") <- NULL
  attr(old_afex_object$anova_table, "p.adjust.method") <- NULL
  expect_that(nice(old_afex_object), is_identical_to(nice(default_options)))
})
