
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
  expect_that(attr(no_attr$anova_table, "p_adjust_method"), equals("none"))
  expect_output(print(attr(no_attr$anova_table, "observed")), "character\\(0\\)")
  
  all_attr <- suppressWarnings(aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), observed = "angle", anova_table=list(correction = "HF", p_adjust_method = "bonferroni")))
  
  expect_that(attr(all_attr$anova_table, "correction"), equals("HF"))
  expect_that(attr(all_attr$anova_table, "p_adjust_method"), equals("bonferroni"))
  expect_that(attr(all_attr$anova_table, "observed"), equals("angle"))
  expect_output(print(all_attr), "bonferroni")
  expect_output(print(all_attr), "HF")
  
  expect_false(isTRUE(all.equal(nice(no_attr), suppressWarnings(nice(all_attr)), check.attributes = FALSE)))
  
  added_attr <- suppressWarnings(nice(no_attr, correction = "HF", p_adjust_method = "bonferroni", observed = "angle"))
  expect_that(suppressWarnings(nice(all_attr)), is_identical_to(added_attr))
  expect_that(nice(all_attr$anova_table), is_identical_to(added_attr))
  
  reset_attr <- nice(no_attr, correction = "none", p.adjust = "none", observed = NULL)
  expect_that(nice(no_attr), is_identical_to(reset_attr))
  expect_that(nice(no_attr$anova_table), is_identical_to(reset_attr))
  
  intercept_test <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), anova_table = list(intercept = TRUE))
  expect_output(print(intercept_test), "(Intercept)")
  
  mse_test <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), anova_table = list(MSE = FALSE))
  expect_null(mse_test$anova_table$MSE)
  expect_output(print(nice(mse_test, MSE = TRUE)), "MSE")
  
  symbol_test <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), anova_table = list(sig_symbols = c(" ", " a", " aa", " aaa")), return = "nice")
  expect_output(print(symbol_test), "aaa")
  
  symbol_test <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), anova_table = list(sig_symbols = c(" ", " a", " aa", " aaa")))
  expect_output(print(symbol_test), "aaa")
  
  new_symbols <- c(" ", " b", " bb", " bbb")
  symbol_test <- anova(symbol_test, sig_symbols = c(" ", " b", " bb", " bbb"))
  expect_identical(attr(symbol_test, "sig_symbols"), new_symbols)
  expect_output(print(nice(symbol_test)), "bbb")
  
  
  # Test support for old afex objects
  old_afex_object <- default_options <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))
  attr(old_afex_object$anova_table, "observed") <- NULL
  attr(old_afex_object$anova_table, "correction") <- NULL
  attr(old_afex_object$anova_table, "p.adjust.method") <- NULL
  expect_that(nice(old_afex_object), is_identical_to(nice(default_options)))
  
  # Test if sphericity correction is set to "none" in the absence of within-subject factors or if within-subject factors have only two levels
  data(obk.long)
  between_anova <- suppressWarnings(aov_car(value ~ treatment * gender + Error(id), data = obk.long))
  expect_that(attr(between_anova$anova_table, "correction"), equals("none"))
  
  obk.long <- droplevels(obk.long[obk.long$phase %in% c("post","pre"),])
  two_level_anova <- suppressWarnings(aov_ez("id", "value", obk.long, between = c("treatment"), within = c("phase")))
  expect_that(attr(two_level_anova$anova_table, "correction"), equals("none"))
  
  more_levels_anova <- aov_ez("id", "value", obk.long, between = c("treatment"), within = c("phase", "hour"))
  expect_that(attr(more_levels_anova$anova_table, "correction"), equals("GG"))
  
  obk.long <- droplevels(obk.long[obk.long$hour %in% c("1","2"),])
  two_levels_anova <- aov_ez("id", "value", obk.long, between = c("treatment"), within = c("phase", "hour"))
  expect_that(attr(two_levels_anova$anova_table, "correction"), equals("none"))
  
  # Test incomplete observation attribute
  incomplete_cases <- suppressWarnings(
    aov_ez("id", "rt", md_12.1[-10, ], within = c("angle", "noise"))
  )
  
  expect_equal(as.character(attr(incomplete_cases, "incomplete_cases")), "10")
  expect_equal(as.character(attr(incomplete_cases$anova_table, "incomplete_cases")), "10")
  expect_equal(as.character(attr(anova(incomplete_cases), "incomplete_cases")), "10")
  
})
