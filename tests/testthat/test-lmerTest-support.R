
context("interplay with lmerTest")

test_that("mixed allows both lme4 and lmerText calls and exports lmerTest::lmer", {
  aop <- afex_options()
  data(sk2011.1)
  m1 <- mixed(response ~ instruction*inference*plausibility +(1|id), sk2011.1, 
              progress = FALSE, return = "merMod")
  m1b <- lmer(response ~ instruction*inference*plausibility +(1|id), sk2011.1)
  expect_true(inherits(m1, "merModLmerTest") || inherits(m1, "lmerModLmerTest"))
  expect_is(m1, "merMod")
  expect_true(inherits(m1b, "merModLmerTest") || inherits(m1b, "lmerModLmerTest"))
  afex_options(lmer_function = "lme4")
  m2 <- mixed(response ~ instruction*inference*plausibility +(1|id), sk2011.1, 
              progress = FALSE, return = "merMod")
  expect_false(inherits(m2, "merModLmerTest") && inherits(m2, "lmerModLmerTest"))
  expect_is(m2, "merMod")
  afex_options(aop)
  expect_true("Pr(>F)" %in% colnames(lmerTest_anova(m1)))
  expect_true("Pr(>F)" %in% colnames(lmerTest_anova(m1b)))
  expect_false("Pr(>F)" %in% colnames(anova(m2)))
  expect_true("Pr(>F)" %in% colnames(lmerTest_anova(m2)))
  
  ## following tests only work with new lmerTest (March 2018)
  pkg_version <- "2.0-37.9005"
  skip_if(packageVersion(pkg = "lmerTest") < pkg_version)
  expect_true("Pr(>F)" %in% colnames(anova(m1)))
  expect_true("Pr(>F)" %in% colnames(anova(m1b)))
  
})

