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

test_that("response variable y works", {
  
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("MEMSS")
  testthat::skip_on_cran()
  
  data("Machines", package = "MEMSS") 

  Machines$y <- rnorm(nrow(Machines), mean = -100)
  ## was a problem if it came before the DV column
  Machines <- Machines[, c("y", "Worker", "Machine", "score")]
  
  # simple model with random-slopes for repeated-measures factor
  m1 <- mixed(score ~ Machine + (1|Worker), data=Machines, progress = FALSE)
  m1
  
  pp <- afex_plot(m1, "Machine")
  
  expect_true(all(pp$layers[[1]]$data$y > 0))
})

test_that("merMod objects with missing data can be plotted", {
  
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("MEMSS")
  testthat::skip_on_cran()
  
  data("Machines", package = "MEMSS") 
  
  Machines[1, "score"] <- NA
  
  m1 <- lme4::lmer(score ~ Machine + (1|Worker), data=Machines)
  pp <- afex_plot(m1, "Machine")
  
  expect_is(pp, "ggplot")
  
  m2 <- mixed(score ~ Machine + (1|Worker), data=Machines, 
              progress = FALSE)
  pp2 <- afex_plot(m2, "Machine")
  
  expect_equivalent(
    pp$layers[[1]]$data, pp2$layers[[1]]$data  
  )
})
