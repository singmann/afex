
context("Mixed LMMs: replicating Maxwell & Delaney (2004)")

test_that("mixed: Maxell & Delaney (2004), Table 16.3, p. 837", {
  ### replicate results from Table 16.3 (Maxwell & Delaney, 2004, p. 837)
  data(md_16.1)  
  # original results need treatment contrasts:
  mixed1_orig <- mixed(severity ~ sex + (1|id), md_16.1, check_contrasts=FALSE, progress=FALSE)
  expect_that(fixef(mixed1_orig$full_model), is_equivalent_to(c(60, -14)))
  expect_that(round(anova(mixed1_orig)[1,"F"],2), equals(9.97))
  expect_that(round(anova(mixed1_orig)[1,"Pr(>F)"],2), equals(0.03))
})

test_that("mixed: Maxell & Delaney (2004), Table 16.6, p. 845", {
  data(md_16.4)
  skip_if_not_installed("Matrix")
  md_16.4b <- within(md_16.4, cond <- C(cond, contr.treatment, base = 2))
  mixed2_orig <- mixed(induct ~ cond + (1|room:cond), md_16.4b, check_contrasts=FALSE, progress=FALSE)
  expect_that(round(fixef(mixed2_orig$full_model), 4), is_equivalent_to(c(35.6261, -8.1485)))
  expect_that(round(sqrt(Matrix::diag(vcov(mixed2_orig$full_model))), 3), equals(c(3.229, 4.548)))
  expect_that(round(mixed2_orig[[1]]$F, 1), equals(3.2))    
})

test_that("mixed: Maxell & Delaney (2004), Table 16.7, p. 851 (uses simple F!)", {
  data(md_16.4)
  skip_if_not_installed("Matrix")
  md_16.4b <- within(md_16.4, cond <- C(cond, contr.treatment, base = 2))
  ### replicate results from Table 16.7 (Maxwell & Delaney, 2004, p. 851)
  # F-values (almost) hold, p-values (especially for skill) are off
  # however, parameters are perfectly recovered when using the original contrasts:
  mixed3_orig <- mixed(induct ~ cond + skill + (1|room:cond), md_16.4b, check_contrasts=FALSE, progress=FALSE)
  expect_that(round(fixef(mixed3_orig$full_model), 2), is_equivalent_to(c(20.25, -7.57, 2.31)))
  expect_that(round(sqrt(Matrix::diag(vcov(mixed3_orig$full_model))), 2), equals(c(5.82, 2.72, 0.81)))
  expect_that(round(mixed3_orig[[1]]$F), equals(c(8, 8)))
  #mixed3_F_simple <- mixed(induct ~ cond + skill + (1|room:cond), md_16.4b, check_contrasts=FALSE, progress=FALSE, method = "F")
  #expect_that(round(fixef(mixed3_F_simple$full_model), 2), is_equivalent_to(c(20.25, -7.57, 2.31)))
  #expect_that(round(sqrt(diag(vcov(mixed3_F_simple$full_model))), 2), equals(c(5.82, 2.72, 0.81)))
  #expect_that(round(mixed3_F_simple[[1]]$F, 1), equals(c(7.8, 8.2)))
})

test_that("mixed: Maxell & Delaney (2004), Table 16.10, p. 862 (does not replicate the table!)", {
  data(md_16.4)
  md_16.4b <- within(md_16.4, cond <- C(cond, contr.treatment, base = 2))
  #note: the values in this test should not replicate the table...
  md_16.4b$cog <- scale(md_16.4b$cog, scale=FALSE)
  mixed4 <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b, progress=FALSE, check_contrasts=FALSE)
  expect_that(round(fixef(mixed4$full_model), 2), is_equivalent_to(c(36.1, -9.07, 0.64, 0.03)))
})
