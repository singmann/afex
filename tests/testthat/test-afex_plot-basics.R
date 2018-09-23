context("afex_plot: basic functionality")

test_that("all input type works and warnings are correct", {
  data(obk.long, package = "afex")
  # estimate mixed ANOVA on the full design:
  a1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
                data = obk.long, observed = "gender")
  
  expect_warning(
    em1 <- afex_plot(a1, ~phase*hour, ~treatment+gender, return = "data"),
    "mixed within-between-design"
  )
  expect_warning(
    em2 <- afex_plot(a1, c("phase", "hour"), ~treatment+gender, 
                     return = "data"),
    "mixed within-between-design"
  )
  expect_warning(
    em3 <- afex_plot(a1, ~phase*hour, c("treatment", "gender"), 
                     return = "data"),
    "mixed within-between-design"
  )
  expect_warning(
    em4 <- afex_plot(a1, c("phase", "hour"), c("treatment", "gender"), 
                     return = "data"),
    "mixed within-between-design"
  )
  
  expect_equal(em1, em2)
  expect_equal(em1, em3)
  expect_equal(em1, em4)
  
  expect_warning(
    em5 <- afex_plot(a1, c("phase", "hour"), return = "data"),
    "show within-subjects factors, but not within-subjects error bars"
  )
  expect_warning(
    em6 <- afex_plot(a1, ~phase*hour, return = "data"),
    "show within-subjects factors, but not within-subjects error bars"
  )
  expect_equal(em5, em6)
  
  expect_warning(
    em7 <- afex_plot(a1, c("treatment", "gender"), panel = "phase", 
                     return = "data", error = "within"),
    "between-subjects factors, but within-subjects error bars"
  )
  expect_warning(
    em8 <- afex_plot(a1, ~treatment*gender, panel = "phase",
                     return = "data", error = "within"),
    "between-subjects factors, but within-subjects error bars"
  )
  expect_equal(em7, em8)
})

test_that("ANOVA plots are produced", {
  data(obk.long, package = "afex")
  a1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
                data = obk.long, observed = "gender")
  
  expect_is(afex_plot(a1, "hour", error = "within"), "ggplot")
  expect_is(afex_plot(a1, c("phase", "hour"), trace = "treatment", 
                      error = "none"), "ggplot")
  expect_is(afex_plot(a1, "phase", trace = "hour",  panel = "treatment",
                      error = "within"), "ggplot")
})

test_that("mixed plots are produced", {
  data("fhch2010") # load 
  fhch <- droplevels(fhch2010[ fhch2010$correct,]) # remove errors
  ### reduced data.frame length
  # fhch <- fhch[unlist(lapply(which(as.numeric(fhch$id) != 
  #                                    c(NA, fhch$id[-length(fhch$id)])), 
  #                            function(x) 0:29 + x)),]
  mrt <- mixed(log_rt ~ task*stimulus*frequency + (1|id), 
               fhch, method = "S")

  expect_is(afex_plot(mrt, "task", 
                      random = "id"), "ggplot")
  expect_is(afex_plot(mrt, x = "stimulus", panel = "task", 
                      random = "id"), "ggplot")
  expect_is(afex_plot(mrt, x = "stimulus", trace = "task", 
                      random = "id"), "ggplot")
  expect_is(afex_plot(mrt, x = "stimulus", trace =  "frequency", panel = "task", 
                      random = "id"), "ggplot")
})

test_that("lme4::merMod plots are produced", {
  Oats <- nlme::Oats
  Oats$VarBlock <- Oats$Variety:Oats$Block
  Oats.lmer <- lmer(yield ~ Variety * factor(nitro) + (1|VarBlock) + (1|Block),
                    data = Oats)
  expect_is(afex_plot(Oats.lmer, "nitro", 
                      random = "VarBlock"), "ggplot")
  expect_is(afex_plot(Oats.lmer, "nitro", "Variety", 
                      random = "VarBlock"), "ggplot")
  expect_is(afex_plot(Oats.lmer, "nitro", panel = "Variety", 
                      random = "VarBlock"), "ggplot")
})

test_that("afex_plot works with various geoms (from examples)", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("ggpol")
  testthat::skip_if_not_installed("ggbeeswarm")
  data(md_12.1)
  aw <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))
  p1 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.3,
                  data_arg = list(
                    position = 
                      ggplot2::position_jitterdodge(
                        jitter.width = 0, 
                        jitter.height = 5, 
                        dodge.width = 0.3  ## needs to be same as dodge
                      ),
                    color = "darkgrey"))
  expect_is(p1, "ggplot")
  
  # 2. using ggbeeswarm::geom_beeswarm
  p2 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.5,
                  data_geom = ggbeeswarm::geom_beeswarm,
                  data_arg = list(
                    dodge.width = 0.5,  ## needs to be same as dodge
                    cex = 0.8,
                    color = "darkgrey"))
  expect_is(p2, "ggplot")
  
  # 3. do not display points, but use a violinplot: ggplot2::geom_violin
  p3 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", 
                  data_geom = ggplot2::geom_violin, 
                  data_arg = list(width = 0.5))
  expect_is(p3, "ggplot")
  
  # 4. violinplots with color: ggplot2::geom_violin
  p4 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", 
                  mapping = c("linetype", "shape", "fill"),
                  data_geom = ggplot2::geom_violin, 
                  data_arg = list(width = 0.5))
  expect_is(p4, "ggplot")
  
  # 5. do not display points, but use a boxplot: ggplot2::geom_boxplot
  p5 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", 
                  data_geom = ggplot2::geom_boxplot, 
                  data_arg = list(width = 0.3))
  expect_is(p5, "ggplot")
  
  # 6. combine points with boxplot: ggpol::geom_boxjitter
  p6 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", 
                  data_geom = ggpol::geom_boxjitter, 
                  data_arg = list(width = 0.3))
  ## hides error bars!
  expect_is(p6, "ggplot")
  
  # 7. nicer variant of ggpol::geom_boxjitter
  p7 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", 
                  mapping = c("shape", "fill"),
                  data_geom = ggpol::geom_boxjitter, 
                  data_arg = list(
                    width = 0.3, 
                    jitter.width = 0,
                    jitter.height = 10,
                    outlier.intersect = TRUE),
                  point_arg = list(size = 2.5), 
                  error_arg = list(size = 1.5, width = 0))
  expect_is(p7, "ggplot")
  
  # 8. nicer variant of ggpol::geom_boxjitter without lines
  p8 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.7,
                  mapping = c("shape", "fill"),
                  data_geom = ggpol::geom_boxjitter, 
                  data_arg = list(
                    width = 0.5, 
                    jitter.width = 0,
                    jitter.height = 10,
                    outlier.intersect = TRUE),
                  point_arg = list(size = 2.5), 
                  line_arg = list(linetype = 0),
                  error_arg = list(size = 1.5, width = 0))
  expect_is(p8, "ggplot")
})
