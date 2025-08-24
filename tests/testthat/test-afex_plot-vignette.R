test_that("glmmTMB object", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  library("glmmTMB")
  set_sum_contrasts()
  tmb2 <- glmmTMB(
    count ~ spp * mined + (1 | site),
    ziformula = ~ spp * mined,
    family = nbinom2,
    Salamanders
  )
  # tmb <- tmb2
  # save(tmb, file = "inst/extdata/tmb_example_fit.rda",
  #      compress = "xz")
  #load(system.file("extdata/", "tmb_example_fit.rda", package = "afex"))
  ## previous versions checked for equivalence of summary() or coef(), but that
  ## seems unnecessary and did fail at some point
  ## (i.e., summary() on old glmmTMB object failed)
  # expect_equivalent(coef(tmb), #summary(tmb),
  #                   coef(tmb2), #summary(tmb2),
  #                   tolerance = 0.001)

  skip_if_not_installed("cowplot")
  skip_if_not_installed("ggplot2")
  library("ggplot2")

  p1n <- afex_plot(tmb2, "spp")
  p2n <- afex_plot(tmb2, "spp", data_geom = geom_violin)
  p3n <- afex_plot(tmb2, "spp", id = "site", data = Salamanders)
  expect_doppelganger("afex_plot: glmmTMB 1", p1n)
  expect_doppelganger("afex_plot: glmmTMB 2", p2n)
  expect_doppelganger("afex_plot: glmmTMB 3", p3n)
})


test_that("rstanarm plots", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  skip_on_cran()
  #skip_on_ci()
  skip_if_not_installed("rstanarm")
  library("rstanarm") ## requires resetting the ggplot2 theme
  skip_if_not_installed("ggplot2")
  library("ggplot2")
  set_sum_contrasts()
  load("afex_plot-rstanarm.rda") ## comparison values

  tol <- 0.1

  cbpp <- lme4::cbpp
  cbpp$prob <- with(cbpp, incidence / size)
  suppressWarnings(capture_output({
    example_model <- rstanarm::stan_glmer(
      prob ~ period + (1 | herd),
      data = cbpp,
      family = binomial,
      weight = size,
      chains = 2,
      cores = 1,
      seed = 12345,
      iter = 1000,
      warmup = 500
    )
  }))

  b1d <- afex_plot(example_model, "period", return = "data")
  expect_equal(b1d, rstanenv$b1d, tolerance = tol)

  ## make cbpp long
  cbpp_l <- vector("list", nrow(cbpp))
  for (i in seq_along(cbpp_l)) {
    cbpp_l[[i]] <- data.frame(
      herd = cbpp$herd[i],
      period = cbpp$period[i],
      incidence = rep(0, cbpp$size[i])
    )
    cbpp_l[[i]]$incidence[seq_len(cbpp$incidence[i])] <- 1
  }
  cbpp_l <- do.call("rbind", cbpp_l)
  cbpp_l$herd <- factor(cbpp_l$herd, levels = levels(cbpp$herd))
  cbpp_l$period <- factor(cbpp_l$period, levels = levels(cbpp$period))

  suppressWarnings(capture_output({
    example_model2 <- rstanarm::stan_glmer(
      incidence ~ period + (1 | herd),
      data = cbpp_l,
      family = binomial,
      chains = 2,
      cores = 1,
      seed = 12345,
      iter = 1000
    )
  }))

  b3d <- afex_plot(example_model2, "period", return = "data")
  b4d <- afex_plot(example_model2, "period", id = "herd", return = "data")
  expect_equal(b3d, rstanenv$b3d, tolerance = tol)
  expect_equal(b4d, rstanenv$b4d, tolerance = tol)

  skip_if_not_installed("MEMSS")
  data("Machines", package = "MEMSS")

  suppressWarnings(capture_output({
    mm <- rstanarm::stan_lmer(
      score ~ Machine + (Machine | Worker),
      data = Machines,
      chains = 2,
      cores = 1,
      seed = 12345,
      iter = 1000
    )
  }))

  b5d <- afex_plot(mm, "Machine", return = "data")
  b6d <- afex_plot(mm, "Machine", id = "Worker", return = "data")
  expect_equal(b5d, rstanenv$b5d, tolerance = tol)
  expect_equal(b6d, rstanenv$b6d, tolerance = tol)

  #### save
  # rstanenv <- new.env()
  # rstanenv$b1d <- b1d
  # rstanenv$b3d <- b3d
  # rstanenv$b4d <- b4d
  # rstanenv$b5d <- b5d
  # rstanenv$b6d <- b6d
  # ls(envir = rstanenv)
  # save(rstanenv, file = "tests/testthat/afex_plot-rstanarm.rda")
})

test_that("brms plots", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  skip_on_cran()
  #skip_on_os("windows")
  skip_if_not_installed("brms")
  library("brms") ## requires resetting the ggplot2 theme
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("MEMSS")
  library("ggplot2")
  set_sum_contrasts()
  data("Machines", package = "MEMSS")
  suppressWarnings(capture_output({
    mm2 <- brm(
      score ~ Machine + (Machine | Worker),
      data = Machines,
      chains = 2,
      cores = 1,
      seed = 12345,
      iter = 1000
    )
  }))
  bb1n <- afex_plot(
    mm2,
    "Machine",
    data = Machines,
    dv = "score",
    return = "data"
  )
  bb2n <- afex_plot(
    mm2,
    "Machine",
    id = "Worker",
    data = Machines,
    dv = "score",
    return = "data"
  )
  load("afex_plot-brms.rda")
  expect_equal(bb1n, brmslist$bb1n, tolerance = 5)
  expect_equal(bb2n, brmslist$bb2n, tolerance = 5)
  # brmslist <- list(
  #   bb1n = bb1n,
  #   bb2n = bb2n
  # )
  # save(brmslist, file = "tests/testthat/afex_plot-brms.rda")

  expect_error(
    afex_plot(mm2, "Machine", data = Machines),
    "Could not detect dv column"
  )
})
