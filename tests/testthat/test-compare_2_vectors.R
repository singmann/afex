
context("compare.2.vectors: known bugs")

test_that("exactly equal mean does not fail", {
  x <- c(0.309, 0.222, 0.293, 0.238, 0.33, 0.215)
  y <- c(0.313, 0.213, 0.306, 0.253, 0.294, 0.228)
  out <- suppressWarnings(compare.2.vectors(x, y, paired = TRUE))
  expect_is(out,"list")
})
