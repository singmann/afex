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
