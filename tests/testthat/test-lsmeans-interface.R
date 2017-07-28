
context("interplay with lsmeans")

test_that("ANOVA functions work with lsmeans", {
  data(sk2011.1)
  a1 <- aov_ez("id", "response", sk2011.1, between = "instruction", within = c("inference", "plausibility"), fun_aggregate = mean)
  expect_is(lsmeans(a1, ~ inference), "lsmobj")
  a2 <- aov_ez("id", "response", sk2011.1, between = "instruction", within = c("inference"), fun_aggregate = mean)
  expect_is(lsmeans(a2, ~ inference), "lsmobj")
  a3 <- aov_ez("id", "response", sk2011.1, within = c("inference"), fun_aggregate = mean)
  expect_is(lsmeans(a3, ~ inference), "lsmobj")
  a4 <- aov_ez("id", "response", sk2011.1, between = "instruction", fun_aggregate = mean)
  expect_is(lsmeans(a4, ~ instruction), "lsmobj")
})


test_that("mixed works with lsmeans", {
  data(sk2011.1)
  m1 <- mixed(response ~ instruction*inference*plausibility +(1|id), sk2011.1, progress = FALSE)
  expect_is(lsmeans(m1, ~ inference), "lsmobj")
  m2 <- mixed(response ~ inference +(inference|id), sk2011.1, progress = FALSE)
  expect_is(lsmeans(m2, ~ inference), "lsmobj")
  m3 <- mixed(response ~ instruction +(inference|id), sk2011.1, progress = FALSE)
  expect_is(lsmeans(m3, ~ instruction), "lsmobj")
})

test_that("mixed works with type=2 and all methods", {
  data(md_16.4)
  mixed_kr <- mixed(induct ~ cond + (1|room:cond), md_16.4, type=2, method="KR", progress = FALSE)
  expect_is(lsmeans(mixed_kr, specs = c("cond")), "lsmobj")
  
  mixed_s <- mixed(induct ~ cond + (1|room:cond), md_16.4, type=2, method="S", progress = FALSE)
  expect_is(lsmeans(mixed_s, specs = c("cond")), "lsmobj")
  
  mixed_lrt <- mixed(induct ~ cond + (1|room:cond), md_16.4, type=2, method="LRT", progress = FALSE)
  expect_is(lsmeans(mixed_lrt, specs = c("cond")), "lsmobj")
  
  mixed_pb <- mixed(induct ~ cond + (1|room:cond), md_16.4, type=2, method="PB", progress = FALSE, 
                    args_test = list(nsim = 10))
  expect_is(lsmeans(mixed_pb, specs = c("cond")), "lsmobj")
  
  mixed_oldkr <- mixed(induct ~ cond + (1|room:cond), md_16.4, type=2, method="nested-KR", progress = FALSE)
  expect_is(lsmeans(mixed_oldkr, specs = c("cond")), "lsmobj")
})