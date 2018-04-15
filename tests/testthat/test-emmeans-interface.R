
context("interplay with emmeans")

test_that("ANOVA functions work with emmeans, univariate & multivariate", {
  data(sk2011.1)
  a1 <- aov_ez("id", "response", sk2011.1, between = "instruction", 
               within = c("inference", "plausibility"), fun_aggregate = mean)
  em1 <- emmeans(a1, ~ inference, model = "univariate")
  em2 <- emmeans(a1, ~ inference, model = "multivariate")
  expect_is(em1, "emmGrid")
  expect_is(em2, "emmGrid")
  expect_equal(as.data.frame(summary(em2))$emmean, 
               as.data.frame(summary(em1))$emmean)
  expect_false(isTRUE(all.equal(
    as.data.frame(summary(em2))$SE, 
    as.data.frame(summary(em1))$SE)))
  
  em1 <- emmeans(a1, ~ inference*plausibility, model = "univariate")
  em2 <- emmeans(a1, ~ inference*plausibility, model = "multivariate")
  expect_is(em1, "emmGrid")
  expect_is(em2, "emmGrid")
  expect_equal(as.data.frame(summary(em2))$emmean, 
               as.data.frame(summary(em1))$emmean)
  expect_false(isTRUE(all.equal(
    as.data.frame(summary(em2))$SE, 
    as.data.frame(summary(em1))$SE)))
  
  em1 <- emmeans(a1, ~ plausibility*inference, model = "univariate")
  em2 <- emmeans(a1, ~ plausibility*inference, model = "multivariate")
  expect_is(em1, "emmGrid")
  expect_is(em2, "emmGrid")
  expect_equal(as.data.frame(summary(em2))$emmean, 
               as.data.frame(summary(em1))$emmean)
  expect_false(isTRUE(all.equal(
    as.data.frame(summary(em2))$SE, 
    as.data.frame(summary(em1))$SE)))
  
  a1b <- aov_ez("id", "response", sk2011.1, between = "instruction", 
               within = c("plausibility", "inference"), fun_aggregate = mean)
  em1 <- emmeans(a1b, ~ inference, model = "univariate")
  em2 <- emmeans(a1b, ~ inference, model = "multivariate")
  expect_is(em1, "emmGrid")
  expect_is(em2, "emmGrid")
  expect_equal(as.data.frame(summary(em2))$emmean, 
               as.data.frame(summary(em1))$emmean)
  expect_false(isTRUE(all.equal(
    as.data.frame(summary(em2))$SE, 
    as.data.frame(summary(em1))$SE)))
  
  a2 <- aov_ez("id", "response", sk2011.1, between = "instruction", 
               within = c("inference"), fun_aggregate = mean)
  em1 <- emmeans(a2, ~ inference, model = "univariate")
  em2 <- emmeans(a2, ~ inference, model = "multivariate")
  expect_is(em1, "emmGrid")
  expect_is(em2, "emmGrid")
  expect_equal(as.data.frame(summary(em2))$emmean, 
               as.data.frame(summary(em1))$emmean)
  expect_false(isTRUE(all.equal(
    as.data.frame(summary(em2))$SE, 
    as.data.frame(summary(em1))$SE)))
  
  
  a3 <- aov_ez("id", "response", sk2011.1, within = c("inference"), 
               fun_aggregate = mean)
  em1 <- emmeans(a3, ~ inference, model = "univariate")
  em2 <- emmeans(a3, ~ inference, model = "multivariate")
  expect_is(em1, "emmGrid")
  expect_is(em2, "emmGrid")
  expect_equal(as.data.frame(summary(em2))$emmean, 
               as.data.frame(summary(em1))$emmean)
  expect_false(isTRUE(all.equal(
    as.data.frame(summary(em2))$SE, 
    as.data.frame(summary(em1))$SE)))
  
  a4 <- aov_ez("id", "response", sk2011.1, between = "instruction", 
               fun_aggregate = mean)
  em1 <- emmeans(a4, ~ instruction, model = "univariate")
  em2 <- emmeans(a4, ~ instruction, model = "multivariate")
  expect_is(em1, "emmGrid")
  expect_is(em2, "emmGrid")
  expect_equal(as.data.frame(summary(em2))$emmean, 
               as.data.frame(summary(em1))$emmean)
  expect_true(isTRUE(all.equal(
    as.data.frame(summary(em2))$SE, 
    as.data.frame(summary(em1))$SE)))
  
  #### 
  data("sk2011.2")
  ab1 <- aov_ez("id", "response", sk2011.2, between = "instruction", 
               within = c("what", "validity", "type"), fun_aggregate = mean)
  em1 <- emmeans(ab1, ~ what*validity*type, model = "univariate")
  em2 <- emmeans(ab1, ~ what*validity*type, model = "multivariate")
  expect_is(em1, "emmGrid")
  expect_is(em2, "emmGrid")
  expect_equal(as.data.frame(summary(em2))$emmean, 
               as.data.frame(summary(em1))$emmean, tolerance = 0.1)
  expect_false(isTRUE(all.equal(
    as.data.frame(summary(em2))$SE, 
    as.data.frame(summary(em1))$SE)))
  
  em1 <- emmeans(ab1, ~ validity*what*type, model = "univariate")
  em2 <- emmeans(ab1, ~ validity*what*type, model = "multivariate")
  expect_is(em1, "emmGrid")
  expect_is(em2, "emmGrid")
  expect_equal(as.data.frame(summary(em2))$emmean, 
               as.data.frame(summary(em1))$emmean, tolerance = 0.1)
  expect_false(isTRUE(all.equal(
    as.data.frame(summary(em2))$SE, 
    as.data.frame(summary(em1))$SE)))
  
  em1 <- emmeans(ab1, ~ type, model = "univariate")
  em2 <- emmeans(ab1, ~ type, model = "multivariate")
  expect_is(em1, "emmGrid")
  expect_is(em2, "emmGrid")
  expect_equal(as.data.frame(summary(em2))$emmean, 
               as.data.frame(summary(em1))$emmean, tolerance = 0.1)
  expect_false(isTRUE(all.equal(
    as.data.frame(summary(em2))$SE, 
    as.data.frame(summary(em1))$SE)))
  
  
})

test_that("ANCOVA with emmeans is correct for univariate & multivariate", {
  data(sk2011.1)
  # a1 <- aov_ez("id", "value", obk.long, between = c("treatment", "gender"),
  #              within = c("phase", "hour"), covariate = "age",
  #              observed = c("gender", "age"), factorize = FALSE)
  # emmeans(a1, ~ phase, model = "multivariate")
  # emmeans(a1, ~ phase, model = "univariate")
  # 
  # emmeans(a1, ~ treatment, model = "multivariate")
  # emmeans(a1, ~ treatment, model = "univariate")
  # deactivated, see: https://github.com/rvlenth/emmeans/issues/32
  
  a2 <- aov_ez("id", "value", obk.long, between = c("treatment", "gender"), 
               covariate = "age", fun_aggregate = mean,
               observed = c("gender", "age"), factorize = FALSE)
  
  em1 <- emmeans(a2, ~ treatment, model = "univariate")
  em2 <- emmeans(a2, ~ treatment, model = "multivariate")
  expect_equal(summary(em1)$emmean, summary(em2)$emmean)
})

test_that("mixed works with emmeans", {
  data(sk2011.1)
  m1 <- mixed(response ~ instruction*inference*plausibility +(1|id), sk2011.1, progress = FALSE)
  expect_is(emmeans(m1, ~ inference), "emmGrid")
  m2 <- mixed(response ~ inference +(inference|id), sk2011.1, progress = FALSE)
  expect_is(emmeans(m2, ~ inference), "emmGrid")
  m3 <- mixed(response ~ instruction +(inference|id), sk2011.1, progress = FALSE)
  expect_is(emmeans(m3, ~ instruction), "emmGrid")
})

test_that("mixed works with type=2 and all methods", {
  emm_options(lmer.df = "asymptotic")
  ## in all tests, data needs to be passed because of nested evaluation.
  
  data("sk2011.2")
  sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])
  
  mixed_kr <- mixed(response ~ inference*type+(1|id), sk2_aff, type=2, 
                    method="KR", progress = FALSE)
  expect_is(emmeans(mixed_kr, specs = c("type"), data = sk2_aff), "emmGrid")
  
  mixed_s <- mixed(response ~ inference*type+(1|id), sk2_aff, type=2, 
                   method="S", progress = FALSE)
  expect_is(emmeans(mixed_s, specs = c("type"), data = sk2_aff), "emmGrid")
  
  mixed_lrt <- mixed(response ~ inference*type+(1|id), sk2_aff, type=2, 
                     method="LRT", progress = FALSE)
  expect_is(emmeans(mixed_lrt, specs = c("type"), data = sk2_aff), "emmGrid")
  
  mixed_pb <- suppressWarnings(mixed(response ~ inference*type+(1|id), sk2_aff, 
                                     type=2, method="PB", progress = FALSE, 
                                     args_test = list(nsim = 10)))
  expect_is(emmeans(mixed_pb, specs = c("type"), data = sk2_aff), "emmGrid")
  
  mixed_oldkr <- mixed(response ~ inference*type+(1|id), sk2_aff, type=2, 
                       method="nested-KR", progress = FALSE)
  expect_is(emmeans(mixed_oldkr, specs = c("type"), data = sk2_aff), "emmGrid")
})

test_that("emmeans works with mixed and expand_er = TRUE", {
  data("Machines", package = "MEMSS") 
  m2 <- mixed(score ~ Machine + (Machine||Worker), data=Machines, 
              expand_re = TRUE, progress = FALSE)
  t1 <- emmeans(m2, "Machine", lmer.df = "asymptotic")
  t2 <- emmeans(m2, "Machine", lmer.df = "Satterthwaite")
  t3 <- emmeans(m2, "Machine", lmer.df = "kenward-roger")
  
  expect_is(t1, "emmGrid")
  expect_is(t2, "emmGrid")
  expect_is(t3, "emmGrid")
  
  expect_is(summary(t1), "data.frame")
  expect_is(summary(t2), "data.frame")
  expect_is(summary(t3), "data.frame")

})



test_that("emmeans with mixed & expand_re = TRUE with pre 3.0 lmerTest objects", {
  load("m_machines_lmerTest-pre3.0.rda")
  # load("tests/testthat/m_machines_lmerTest-pre3.0.rda")
  t1 <- emmeans(m_machines, "Machine", lmer.df = "asymptotic")
  t2 <- emmeans(m_machines, "Machine", lmer.df = "Satterthwaite")
  t3 <- emmeans(m_machines, "Machine", lmer.df = "kenward-roger")
  
  expect_is(t1, "emmGrid")
  expect_is(t2, "emmGrid")
  expect_is(t3, "emmGrid")
  
  expect_is(summary(t1), "data.frame")
  expect_is(summary(t2), "data.frame")
  expect_is(summary(t3), "data.frame")

})
