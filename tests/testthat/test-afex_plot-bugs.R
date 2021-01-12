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


test_that("binomial models plot data correctly with factor DVs", {
  ## long format binomial GLM (https://stats.stackexchange.com/q/322038/442):
  drc4 <- function(x, b =1.0, c = 0, d = 1, e = 0){
    (d - c)/ (1 + exp(-b * (log(x)  - log(e))))
  }
  # simulate long form of dataset
  nReps = 30
  dfLong <- data.frame(dose = factor(rep(letters[1:3], each = nReps)))
  dfLong$mortality <-rbinom(n = dim(dfLong)[1], size = 1,
                            prob = drc4(as.numeric(dfLong$dose), b = 2, e = 5))
  
  fitLong <- glm( mortality ~ dose, data = dfLong, 
                  family = "binomial")
  p1 <- afex_plot(fitLong, "dose")
  dfLong$mortality <- factor(dfLong$mortality)
  fitLong2 <- glm( mortality ~ dose, data = dfLong, 
                   family = "binomial")
  
  
  p2 <- afex_plot(fitLong2, "dose")
  expect_equivalent(p1, p2, check.environment=FALSE)
})
