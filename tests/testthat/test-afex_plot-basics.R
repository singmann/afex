context("afex_plot: basic functionality")

data(obk.long, package = "afex")
# estimate mixed ANOVA on the full design:
a1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
              data = obk.long, observed = "gender")

test_that("all input type works and warnings are correct", {
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
               fhch, method = "S", progress = FALSE)
  
  p1 <- afex_plot(mrt, "task", id = "id")

  expect_is(p1, "ggplot")
  expect_equal(p1$labels$y, "log_rt")
  
  p2 <- afex_plot(mrt, x = "stimulus", panel = "task", 
                      id = "id")
  expect_is(p2, "ggplot")
  expect_equal(p2$labels$y, "log_rt")
  
  p3 <- afex_plot(mrt, x = "stimulus", trace = "task", 
                      id = "id")
  expect_is(p3, "ggplot")
  expect_equal(p3$labels$y, "log_rt")
  
  p4 <- afex_plot(mrt, x = "stimulus", trace =  "frequency", panel = "task", 
                      id = "id")
  expect_is(p4, "ggplot")
  expect_equal(p4$labels$y, "log_rt")
})

test_that("lme4::merMod plots are produced", {
  Oats <- nlme::Oats
  Oats$VarBlock <- Oats$Variety:Oats$Block
  Oats.lmer <- lmer(yield ~ Variety * factor(nitro) + (1|VarBlock) + (1|Block),
                    data = Oats)
  p1 <- afex_plot(Oats.lmer, "nitro", 
                      id = "VarBlock")
  expect_is(p1, "ggplot")
  expect_equal(p1$labels$y, "yield")
  
  expect_is(afex_plot(Oats.lmer, "nitro", "Variety", 
                      id = "VarBlock"), "ggplot")
  expect_is(afex_plot(Oats.lmer, "nitro", panel = "Variety", 
                      id = "VarBlock"), "ggplot")
  
  ## check that id argument works:
  d1 <- afex_plot(Oats.lmer, "nitro", 
                  id = "VarBlock", 
                  return = "data")
  d2 <- afex_plot(Oats.lmer, "nitro", 
                  id = "Block", 
                  return = "data")
  d3 <- afex_plot(Oats.lmer, "nitro", 
                  id = c("Block", "VarBlock"), 
                  return = "data")
  expect_lt(nrow(d2$data), nrow(d1$data))
  expect_lt(nrow(d2$data), nrow(d3$data))
  expect_identical(nrow(d1$data), nrow(d3$data))
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
                    jitter.params = list(width = 0, height = 10),
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
                    jitter.params = list(width = 0, height = 10),
                    outlier.intersect = TRUE),
                  point_arg = list(size = 2.5), 
                  line_arg = list(linetype = 0),
                  error_arg = list(size = 1.5, width = 0))
  expect_is(p8, "ggplot")
})

test_that("relabeling of factors and legend works", {
  data(md_12.1)
  aw <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))
  ## relabel factor levels via new_levels
  p1 <- afex_plot(aw, x = "noise", trace = "angle", error = "within",
                  factor_levels = list(angle = c("0", "4", "8"),
                                       noise = c("Absent", "Present")))
  expect_equal(levels(p1$data$noise), c("Absent", "Present"))
  expect_equal(levels(p1$data$angle), c("0", "4", "8"))
  
  p2 <- afex_plot(aw, x = "noise", trace = "angle", error = "within",
                  factor_levels = list(
                    angle = c(X8 = "8", X4 = "4", X0 = "0"),
                    noise = c(present = "Present")))
  expect_equal(levels(p2$data$angle), rev(c("0", "4", "8")))
  expect_equal(levels(p2$data$noise), c("absent", "Present"))
  
  p1d <- afex_plot(aw, x = "noise", trace = "angle", error = "within",
                  factor_levels = list(angle = c("0", "4", "8"),
                                       noise = c("Absent", "Present")), 
                  return = "data")
  p2d <- afex_plot(aw, x = "noise", trace = "angle", error = "within",
                  factor_levels = list(
                    angle = c(X8 = "8", X4 = "4", X0 = "0"),
                    noise = c(present = "Present")), 
                  return = "data")
  expect_equal(p1d$means$lower, p2d$means$lower)
  
  expect_warning(p3 <- afex_plot(aw, x = "noise", trace = "angle", error = "mean",
                  factor_levels = list(
                    angle = c(X8 = "8", X4 = "4", X0 = "0"),
                    noise = c(present = "Present"))), 
                 "show within-subjects factors, but not within-subjects error bars")
  expect_equal(levels(p3$data$angle), rev(c("0", "4", "8")))
  
  expect_warning(p3d <- afex_plot(aw, x = "noise", trace = "angle", error = "mean",
                  factor_levels = list(
                    angle = c(X8 = "8", X4 = "4", X0 = "0"),
                    noise = c(present = "Present")), 
                  return = "data"), 
                 "show within-subjects factors, but not within-subjects error bars")
  
  expect_warning(p3nd <- afex_plot(aw, x = "noise", trace = "angle", error = "mean",
                  factor_levels = list(angle = c("0", "4", "8"),
                                       noise = c("Absent", "Present")), 
                  return = "data"), 
                 "show within-subjects factors, but not within-subjects error bars")
  expect_equal(p3d$means$lower, p3nd$means$lower)
  
  expect_warning(p4d <- afex_plot(aw, x = "noise", trace = "angle", 
                                  error = "between",
                                  factor_levels = list(
                                    angle = c(X8 = "8", X4 = "4", X0 = "0"),
                                    noise = c(present = "Present")), 
                                  return = "data"), 
                 "show within-subjects factors, but not within-subjects error bars")
  
  expect_warning(p4nd <- afex_plot(aw, x = "noise", trace = "angle", 
                                   error = "between",
                                   factor_levels = list(angle = c("0", "4", "8"),
                                                        noise = c("Absent", "Present")), 
                                   return = "data"), 
                 "show within-subjects factors, but not within-subjects error bars")
  expect_equal(p4d$means$lower, p4nd$means$lower)
  
  expect_error(
    afex_plot(aw, x = "noise", trace = "angle", error = "within",
                  factor_levels = list(angle = c("0", "4"),
                                       noise = c("Absent", "Present"))), 
    "length of new factor_levels for 'angle' != length of factor levels"
  )
  
  
  p2 <- afex_plot(aw, x = "noise", trace = "angle", error = "within",
                  legend_title = "Noise Condition")
  expect_equal(p2$guides$shape$title, "Noise Condition")
  expect_equal(p2$guides$linetype$title, "Noise Condition")
})

test_that("labels are correct in case variables are of lenth > 1", {
  data(obk.long, package = "afex")
  # estimate mixed ANOVA on the full design:
  a1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
                data = obk.long, observed = "gender")
  
  p1 <- afex_plot(a1, c("phase", "hour"), c("treatment", "gender"), 
                  error = "none")
  p2 <- afex_plot(a1, c("phase", "hour"), error = "none")
  expect_match(p1$labels$x, "phase")
  expect_match(p1$labels$x, "hour")
  expect_match(p1$guides$shape$title, "treatment")
  expect_match(p1$guides$shape$title, "gender")
  expect_match(p2$labels$x, "phase")
  expect_match(p2$labels$x, "hour")
})
