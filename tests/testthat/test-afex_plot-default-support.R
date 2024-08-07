context("afex_plot: default method supported models")

test_that("lm works", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  p1 <- afex_plot(warp.lm, "tension")
  d1 <- afex_plot(warp.lm, "tension", return = "data")
  p2 <- afex_plot(warp.lm, "tension", "wool")
  d2 <- afex_plot(warp.lm, "tension", "wool", return = "data")
  expect_doppelganger("afex_plots: lm model 1", p1)
  expect_doppelganger("afex_plots: lm model 2", p2)
  expect_equal(nrow(d2$data), nrow(warpbreaks))
  expect_equal(nrow(d1$data), nrow(warpbreaks))
})

test_that("poisson glm works", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_on_cran() ## uses only expect_doppelganger
  
  ins <- data.frame(
    n = c(500, 1200, 100, 400, 500, 300),
    size = factor(rep(1:3,2), labels = c("S","M","L")),
    age = factor(rep(1:2, each = 3)),
    claims = c(42, 37, 1, 101, 73, 14))
  ins.glm <- glm(claims ~ size + age + offset(log(n)), 
                 data = ins, family = "poisson")
  p1 <- afex_plot(ins.glm, "size")
  p2 <- afex_plot(ins.glm, "size", "age")
  expect_doppelganger("afex_plots: poisson glm 1", p1)
  expect_doppelganger("afex_plots: poisson glm 2", p2)
})

test_that("binomial glm works", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_on_cran() ## uses only expect_doppelganger
  
  ldose <- factor(rep(0:5, 2))
  numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
  sex <- factor(rep(c("M", "F"), c(6, 6)))
  SF <- numdead/20  ## dv should be a vector, no matrix
  budworm.lg <- glm(SF ~ sex*ldose, family = binomial, 
                    weights = rep(20, length(numdead)))
  p1 <- afex_plot(budworm.lg, "ldose")
  p2 <- afex_plot(budworm.lg, "ldose", "sex") ## data point is hidden behind mean!
  expect_doppelganger("afex_plots: binomial glm 1", p1)
  expect_doppelganger("afex_plots: binomial glm 2", p2)
})

test_that("nlme works", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  skip_if_not_installed("nlme")
  data(Oats, package = "nlme")
  Oats$nitro <- factor(Oats$nitro)
  oats.1 <- nlme::lme(yield ~ nitro * Variety, 
                      random = ~ 1 | Block / Variety,
                      data = Oats)
  p1 <- afex_plot(oats.1, "nitro", "Variety", data = Oats)
  p2 <- afex_plot(oats.1, "nitro", "Variety", data = Oats, id = "Block")
  p3 <- afex_plot(oats.1, "nitro", data = Oats)
  p4 <- afex_plot(oats.1, "nitro", data = Oats, id = c("Block", "Variety"))
  p5 <- afex_plot(oats.1, "nitro", data = Oats, id = "Block")
  expect_doppelganger("afex_plots: nlme 1", p1)
  expect_doppelganger("afex_plots: nlme 2", p2)
  expect_doppelganger("afex_plots: nlme 3", p3)
  expect_doppelganger("afex_plots: nlme 4", p4)
  expect_doppelganger("afex_plots: nlme 5", p5)
  
  d3 <- afex_plot(oats.1, "nitro", data = Oats, return = "data")
  d4 <- afex_plot(oats.1, "nitro", data = Oats, 
                  id = c("Block", "Variety"), return = "data")
  d5 <- afex_plot(oats.1, "nitro", data = Oats, id = "Block", 
                  return = "data")
  expect_equal(nrow(d3$data), nrow(Oats))
  expect_equal(nrow(d4$data), nrow(Oats))
  expect_true(nrow(d5$data) < nrow(Oats))
})

