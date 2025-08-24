test_that("mixed models with Type = 2 work", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("MEMSS")
  testthat::skip_on_cran()
  data("Machines", package = "MEMSS")

  # simple model with random-slopes for repeated-measures factor
  m1 <- mixed(
    score ~ Machine + (1 | Worker),
    data = Machines,
    type = 2,
    method = "LRT",
    progress = FALSE
  )
  expect_doppelganger("afex_plot lmm, type = 2", afex_plot(m1, "Machine"))
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
  m1 <- mixed(score ~ Machine + (1 | Worker), data = Machines, progress = FALSE)
  m1

  pp <- afex_plot(m1, "Machine")

  expect_doppelganger("afex_plot dv = y works", pp)
})

test_that("merMod objects with missing data can be plotted", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("MEMSS")
  testthat::skip_on_cran()

  data("Machines", package = "MEMSS")

  Machines[1, "score"] <- NA

  m1 <- lme4::lmer(score ~ Machine + (1 | Worker), data = Machines)
  pp <- afex_plot(m1, "Machine")

  expect_is(pp, "ggplot")

  suppressWarnings(
    m2 <- mixed(
      score ~ Machine + (1 | Worker),
      data = Machines,
      progress = FALSE
    )
  )
  pp2 <- afex_plot(m2, "Machine")

  expect_doppelganger("afex_plot merMod objects with missing data", pp2)
})


test_that("binomial models plot data correctly with factor DVs", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("emmeans")
  set.seed(898765)
  ## long format binomial GLM (https://stats.stackexchange.com/q/322038/442):
  drc4 <- function(x, b = 1.0, c = 0, d = 1, e = 0) {
    (d - c) / (1 + exp(-b * (log(x) - log(e))))
  }
  # simulate long form of dataset
  nReps = 30
  dfLong <- data.frame(dose = factor(rep(letters[1:3], each = nReps)))
  dfLong$mortality <- rbinom(
    n = dim(dfLong)[1],
    size = 1,
    prob = drc4(as.numeric(dfLong$dose), b = 2, e = 5)
  )

  fitLong <- glm(mortality ~ dose, data = dfLong, family = "binomial")
  p1 <- afex_plot(fitLong, "dose")
  expect_doppelganger("afex_plot binomial glm with factor", p1)
  dfLong$mortality <- factor(dfLong$mortality)
  fitLong2 <- glm(mortality ~ dose, data = dfLong, family = "binomial")
  p2 <- afex_plot(fitLong2, "dose")
  expect_doppelganger("afex_plot binomial glm with factor 2", p2)
  expect_equivalent(p1, p2, check.environment = FALSE)
})

test_that("non-factor IVs work with factor_levels argument", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("MEMSS")
  data("Machines", package = "MEMSS")

  ## transform Machine to character and disable check_contrasts
  Machines$Machine <- as.character(Machines$Machine)
  m2 <- mixed(
    score ~ Machine + (Machine || Worker),
    data = Machines,
    expand_re = TRUE,
    check_contrasts = FALSE,
    progress = FALSE
  )

  df_out <- afex_plot(
    m2,
    "Machine",
    factor_levels = list(Machine = list("B" = "beta", "A" = "a", "C" = "c")),
    return = "data"
  )
  expect_s3_class(df_out$data$Machine, "factor")
  expect_s3_class(df_out$means$Machine, "factor")
})
