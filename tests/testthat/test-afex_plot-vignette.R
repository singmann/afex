context("afex_plot vignette: saved objects replicate")

test_that("glmmTMB object", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  library("glmmTMB")
  set_sum_contrasts()
  tmb2 <- glmmTMB(count~spp * mined + (1|site), 
                 ziformula = ~spp * mined, 
                 family=nbinom2, Salamanders)
  load(system.file("extdata/", "tmb_example_fit.rda", package = "afex"))
  expect_equivalent(tmb, tmb2, tolerance = 0.001)
  
  skip_if_not_installed("cowplot")
  skip_if_not_installed("ggplot2")
  library("ggplot2")
  po <- cowplot::plot_grid(
    afex_plot(tmb, "spp"),
    afex_plot(tmb, "spp", data_geom = geom_violin),
    afex_plot(tmb, "spp", id = "site", data = Salamanders), 
    labels = "AUTO", nrow = 1
  )
  pn <- cowplot::plot_grid(
    afex_plot(tmb2, "spp"),
    afex_plot(tmb2, "spp", data_geom = geom_violin),
    afex_plot(tmb2, "spp", id = "site", data = Salamanders), 
    labels = "AUTO", nrow = 1
  )
  expect_equivalent(po$data, pn$data)
})


test_that("rstanarm plots", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  library("rstanarm") ## requires resetting the ggplot2 theme
  skip_if_not_installed("ggplot2")
  library("ggplot2")
  set_sum_contrasts()
  cbpp <- lme4::cbpp 
  cbpp$prob <- with(cbpp, incidence / size)
  capture_output({
  example_model <- rstanarm::stan_glmer(prob ~ size + period + (1|herd),
                                        data = cbpp, family = binomial, weight = size,
                                        chains = 2, cores = 1, seed = 12345, iter = 500)
  })
  b1n <- afex_plot(example_model, "period")
  b2n <- afex_plot(example_model, "period", data_geom = geom_violin)
  
  skip_if_not_installed("MEMSS")
  data("Machines", package = "MEMSS") 
  
  suppressWarnings(capture_output({
    mm <- rstanarm::stan_lmer(score ~ Machine + (Machine|Worker), data=Machines,
                            chains = 2, cores = 1, seed = 12345, iter = 500)
  }))
  
  b3n <- afex_plot(mm, "Machine")
  b4n <- afex_plot(mm, "Machine", id = "Worker", data = Machines)
  load(system.file("extdata/", "plots_rstanarm.rda", package = "afex"))
  
  expect_equivalent(b1, b1n, tolerance = 0.1)
  expect_equivalent(b2, b2n, tolerance = 0.1)
  expect_equivalent(b3, b3n, tolerance = 0.1)
  expect_equivalent(b4, b4n, tolerance = 0.1)
})

test_that("brms plots", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("brms")
  library("brms") ## requires resetting the ggplot2 theme
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("MEMSS")
  library("ggplot2")
  set_sum_contrasts()
  data("Machines", package = "MEMSS") 
  suppressWarnings(capture_output({
    mm2 <- brm(score ~ Machine + (Machine|Worker), data=Machines, 
               chains = 2, cores = 1, seed = 12345, iter = 500)
  }))
  bb1n <- afex_plot(mm2, "Machine", data = Machines, dv = "score")
  bb2n <- afex_plot(mm2, "Machine", id = "Worker", 
                   data = Machines, dv = "score")
  load(system.file("extdata/", "plots_brms.rda", package = "afex"))
  
  expect_equivalent(bb1, bb1n, tolerance = 0.1)
  expect_equivalent(bb2, bb2n, tolerance = 0.1)
})
