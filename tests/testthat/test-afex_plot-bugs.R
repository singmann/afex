context("afex_plot: known bugs")

test_that("mixed models with Type = 2 work", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("MEMSS")
  testthat::skip_on_cran()
  data("Machines", package = "MEMSS") 

  # simple model with random-slopes for repeated-measures factor
  m1 <- mixed(score ~ Machine + (1|Worker), 
              data=Machines, type = 2, method = "LRT", 
              progress = FALSE)
  expect_is(afex_plot(m1, "Machine"), "ggplot")
})

