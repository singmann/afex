
context("interplay with emmeans")

test_that("ANOVA functions work with emmeans", {
  data(sk2011.1)
  a1 <- aov_ez("id", "response", sk2011.1, between = "instruction", within = c("inference", "plausibility"), fun_aggregate = mean)
  expect_is(emmeans(a1, ~ inference), "emmGrid")
  a2 <- aov_ez("id", "response", sk2011.1, between = "instruction", within = c("inference"), fun_aggregate = mean)
  expect_is(emmeans(a2, ~ inference), "emmGrid")
  a3 <- aov_ez("id", "response", sk2011.1, within = c("inference"), fun_aggregate = mean)
  expect_is(emmeans(a3, ~ inference), "emmGrid")
  a4 <- aov_ez("id", "response", sk2011.1, between = "instruction", fun_aggregate = mean)
  expect_is(emmeans(a4, ~ instruction), "emmGrid")
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
  
  mixed_pb <- suppressWarnings(mixed(response ~ inference*type+(1|id), sk2_aff, type=2, 
                    method="PB", progress = FALSE, 
                    args_test = list(nsim = 10)))
  expect_is(emmeans(mixed_pb, specs = c("type"), data = sk2_aff), "emmGrid")
  
  mixed_oldkr <- mixed(response ~ inference*type+(1|id), sk2_aff, type=2, 
                       method="nested-KR", progress = FALSE)
  expect_is(emmeans(mixed_oldkr, specs = c("type"), data = sk2_aff), "emmGrid")
})