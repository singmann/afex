
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
