test_that("glmmTMB object", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  library("glmmTMB")
  set_sum_contrasts()
  tmb2 <- glmmTMB(count~spp * mined + (1|site), 
                 ziformula = ~spp * mined, 
                 family=nbinom2, Salamanders)
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
  skip_on_ci()
  skip_if_not_installed("rstanarm")
  library("rstanarm") ## requires resetting the ggplot2 theme
  skip_if_not_installed("ggplot2")
  library("ggplot2")
  skip_if_not_installed("cowplot")
  library("cowplot")
  theme_set(theme_bw(base_size = 14) + 
              theme(legend.position="bottom", 
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank()))
  set_sum_contrasts()
  cbpp <- lme4::cbpp 
  cbpp$prob <- with(cbpp, incidence / size)
  suppressWarnings(capture_output({
  example_model <- rstanarm::stan_glmer(prob ~ period + (1|herd),
                                        data = cbpp, family = binomial, weight = size,
                                        chains = 2, cores = 1, seed = 12345, 
                                        iter = 1000, warmup = 500)
  }))
  b1n <- afex_plot(example_model, "period")
  b2n <- afex_plot(example_model, "period", data_geom = geom_violin)
  expect_doppelganger("afex_plot: rstanarm 1", b1n)
  expect_doppelganger("afex_plot: rstanarm 2", b2n)
  
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
    example_model2 <- rstanarm::stan_glmer(incidence ~ period + (1|herd),
                                           data = cbpp_l, family = binomial, 
                                           chains = 2, cores = 1, seed = 12345, 
                                           iter = 1000)
  }))
  
  b3n <- afex_plot(example_model2, "period")
  b4n <- afex_plot(example_model2, "period", id = "herd")
  # b3n <- afex_plot(example_model2, "period", id = "herd", data = cbpp_l)
  expect_doppelganger("afex_plot: rstanarm 3", b3n)
  expect_doppelganger("afex_plot: rstanarm 4", b4n)
  
  skip_if_not_installed("MEMSS")
  data("Machines", package = "MEMSS") 
  
  suppressWarnings(capture_output({
    mm <- rstanarm::stan_lmer(score ~ Machine + (Machine|Worker), data=Machines,
                            chains = 2, cores = 1, seed = 12345, iter = 1000)
  }))
  
  b5n <- afex_plot(mm, "Machine")
  b6n <- afex_plot(mm, "Machine", id = "Worker")
  
  expect_doppelganger("afex_plot: rstanarm 5", b5n)
  expect_doppelganger("afex_plot: rstanarm 6", b6n)
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
    mm2 <- brm(score ~ Machine + (Machine|Worker), data=Machines, 
               chains = 2, cores = 1, seed = 12345, iter = 1000)
  }))
  bb1n <- afex_plot(mm2, "Machine", data = Machines, dv = "score")
  bb2n <- afex_plot(mm2, "Machine", id = "Worker", 
                   data = Machines, dv = "score")
  expect_doppelganger("afex_plot: brms 1", bb1n)
  expect_doppelganger("afex_plot: brms 2", bb2n)
  
  expect_error(afex_plot(mm2, "Machine", data = Machines), 
               "Could not detect dv column")
  
})
