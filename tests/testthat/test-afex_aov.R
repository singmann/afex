
context("ANOVAs: check that afex_aov return value works")

test_that("split-plot produces an afex_aov object without error", {
  data(obk.long, package = "afex")
  split_plot1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
                         data = obk.long, observed = "gender", 
                         return = "afex_aov")
  split_plot2 <- aov_4(value ~ treatment * gender + (phase*hour|id), 
                       data = obk.long, observed = "gender", 
                       return = "afex_aov")
  split_plot3 <- aov_ez("id", "value", obk.long, 
                        between = c("treatment", "gender"), 
                        within = c("phase", "hour"), observed = "gender", 
                        return = "afex_aov")
  
  expect_that(split_plot1, is_equivalent_to(split_plot2))
  expect_that(split_plot1, is_equivalent_to(split_plot3))
  expect_that(split_plot1, is_a("afex_aov"))
  
  ## is same with numeric factor:
  obk.long$hour <- as.numeric(as.character(obk.long$hour))
  split_plot4 <- aov_car(value ~ treatment * gender + Error(id/phase*hour), 
                         data = obk.long,observed = c("gender"), 
                         return = "afex_aov")
  expect_that(split_plot1, is_equivalent_to(split_plot4))  
})

test_that("purely-between produces afex_aov objects without error", {
  data(obk.long, package = "afex")
  out1 <- aov_car(value ~ treatment * gender + Error(id), data = obk.long, 
                  observed = "gender", return = "afex_aov", 
                  fun_aggregate = mean)
  out2 <- aov_4(value ~ treatment * gender + (1|id), data = obk.long, 
                observed = "gender", return = "afex_aov", fun_aggregate = mean)
  out3 <- aov_ez("id", "value", obk.long, 
                 between = c("treatment", "gender"), observed = "gender", 
                 return = "afex_aov", fun_aggregate = mean)
  
  expect_that(out1, is_equivalent_to(out2))
  expect_that(out1, is_equivalent_to(out3))
  expect_that(out1, is_a("afex_aov"))
})

test_that("purely-within produces afex_aov objects without error", {
  data(obk.long, package = "afex")
  out1 <- aov_car(value ~ Error(id/(phase*hour)), data = obk.long, 
                  return = "afex_aov")
  out2 <- aov_4(value ~ 1 +  (phase*hour|id), data = obk.long, 
                return = "afex_aov")
  out3 <- aov_ez("id", "value", obk.long, within = c("phase", "hour"), 
                 return = "afex_aov")
  
  expect_that(out1, is_equivalent_to(out2))
  expect_that(out1, is_equivalent_to(out3))
  expect_that(out1, is_a("afex_aov"))
})

test_that("within plus covariate produces afex_aov objects without error", {
  data(obk.long, package = "afex")
  out1 <- aov_car(value ~ gender + Error(id/(phase*hour)), data = obk.long, 
                  return = "afex_aov")
  out2 <- aov_4(value ~ gender +  (phase*hour|id), data = obk.long, 
                return = "afex_aov")
  out3 <- aov_ez("id", "value", obk.long, within = c("phase", "hour"), 
                 covariate = "gender", return = "afex_aov")
  
  expect_that(out1, is_equivalent_to(out2))
  expect_that(out1, is_equivalent_to(out3))
  expect_that(out1, is_a("afex_aov"))
})


test_that("afex_aov object contains the right things", {
  data(obk.long, package = "afex")
  out1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
                  data = obk.long, observed = "gender", return = "afex_aov")

  expect_that(out1[["anova_table"]], is_a(c("anova", "data.frame")))  
  expect_that(out1[["aov"]], is_a(c("aovlist", "listof")))  
  expect_that(out1[["Anova"]], is_a(c("Anova.mlm")))
  expect_that(out1[["lm"]], is_a(c("mlm", "lm")))  
  expect_that(out1[["data"]], is_a(c("list")))  
  expect_that(attr(out1, "dv"), is_a(c("character")))  
})

test_that("afex_aov objects works without aov object", {
  data(obk.long, package = "afex")
  
  a1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
        data = obk.long, observed = "gender", include_aov = FALSE)
  a2 <- aov_4(value ~ treatment * gender + (phase*hour|id), 
              data = obk.long, observed = "gender", include_aov = FALSE)
  a3 <- aov_ez("id", "value", obk.long, between = c("treatment", "gender"), 
               within = c("phase", "hour"), observed = "gender", 
               include_aov = FALSE)
  expect_equal(a1, a2)
  expect_equal(a1, a3)
  expect_null(a1$aov)
  
  skip_if_not_installed("emmeans")
  expect_message(em1 <- emmeans::emmeans(a1, "treatment"), "multivariate")
  
  expect_message(em2 <- emmeans::emmeans(a1, c("phase", "hour")), "multivariate")
  
  expect_identical(as.data.frame(summary(em1))$df[1], 
                   as.data.frame(summary(em2))$df[2])
  
  op <- afex_options()
  ad <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
        data = obk.long, observed = "gender")
  afex_options(include_aov = FALSE)
  an <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
        data = obk.long, observed = "gender")
  expect_null(an$aov)
  expect_is(ad$aov, "aovlist")
  em3 <- emmeans::emmeans(ad, c("phase", "hour"))
  expect_message(em4 <- emmeans::emmeans(a1, c("phase", "hour")), "multivariate")
  expect_false(any(as.data.frame(summary(em3))$df == as.data.frame(summary(em4))$df))
  afex_options(op)
})

test_that("better error message in case of all data having NAs", {
  data("stroop")
  # stroop_e1 <- stroop %>%
  #   filter(!is.na(acc)) %>% 
  #   filter(study == "1") %>% 
  #   droplevels()
  stroop_e1_na <- stroop[ stroop$study == "1", ]
  
  suppressWarnings(expect_error(aov_ez(
    id = "pno", 
    dv = "acc", 
    data = stroop_e1_na,
    within = c("congruency", "condition")
  ), "Try adding to ANOVA call: na.rm = TRUE"))
  
  expect_is(aov_ez(
    id = "pno", 
    dv = "acc", 
    data = stroop_e1_na,
    within = c("congruency", "condition"), 
    na.rm = TRUE, 
    include_aov = FALSE, 
    fun_aggregate = mean
  ), "afex_aov")
})
