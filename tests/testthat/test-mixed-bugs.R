

context("mixed: known bugs")

test_that("inverse.gaussian works", {
  ## see: https://github.com/singmann/afex/issues/74
  skip_if_not_installed("statmod")
  skip_if(packageVersion("lme4") <= "1.1.21")
  skip_on_cran()
  set.seed(666)
  
  id <- factor(1:20)
  a <- factor(rep(c("a1","a2"),each=5000/2))
  b <- factor(rep(c("b1","b2"),each=5000/2))
  y <- statmod::rinvgauss(5000, 1, 2)
  df <- data.frame(id=id,
                   x1=sample(a),
                   x2=sample(b),
                   y=y)
  
  expect_is(mixed(y ~ x1 * x2 
                     + (1|id),
                     family = inverse.gaussian(link = "inverse"),
                     control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                     data = df,
                     method = "LRT", progress = FALSE), "mixed")
  
  expect_is(mixed(y ~ x1 * x2 + (1|id),
                     family = inverse.gaussian(link = "inverse"),
                     control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                     data = df,
                     method = "PB", progress = FALSE, 
                  args_test = list(nsim = 5)), "mixed")
  
  
})

test_that("character formula is contrast checked", {
  data("sk2011.2")
  # use only affirmation problems (S&K also splitted the data like this)
  sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])
  sk_m1 <- mixed("response ~ instruction*inference+(1|id)", 
                 sk2_aff, method = "S", progress = FALSE)
  
  sk_m2 <- mixed(response ~ instruction*inference+(1|id), sk2_aff, 
                 method = "S", progress = FALSE)
  expect_equivalent(fixef(sk_m1$full_model), fixef(sk_m2$full_model))
  
})

test_that("character variables are treated as factors", {
  data("sk2011.2")
  # use only affirmation problems (S&K also splitted the data like this)
  sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])
  sk_m1 <- mixed(response ~ instruction*inference+(1|id), sk2_aff, 
                 method = "LRT", progress = FALSE)
  
  sk2_aff$instruction <- as.character(sk2_aff$instruction)
  sk2_aff$inference <- as.character(sk2_aff$inference)
  sk_m2 <- mixed(response ~ instruction*inference+(1|id), sk2_aff, 
                 method = "LRT", progress = FALSE)
  expect_equivalent(anova(sk_m1), anova(sk_m2))
  
})

test_that("mixed works with long formulas", {
  data(obk.long)
  obk2 <- obk.long
  colnames(obk2) <- sapply(colnames(obk2), function(x) paste0(x, x, x, x, x, x))
  expect_is(mixed(valuevaluevaluevaluevaluevalue ~ treatmenttreatmenttreatmenttreatmenttreatmenttreatment * phasephasephasephasephasephase * hourhourhourhourhourhour + (1|idididididid), obk2, method = "LRT", progress = FALSE), "mixed")
})

test_that("nice.mixed and print.mixed can handle old objects", {
  # created via:
  #   require(devtools)
  #   dev_mode()
  #   install_url("https://cran.rstudio.com/src/contrib/Archive/afex/afex_0.13-145.tar.gz")
  #   require(afex)
  #   data(obk.long)
  #   m1 <- mixed(value ~ treatment * phase + (1|id), obk.long)
  #   m2 <- mixed(value ~ treatment * phase + (1|id), obk.long, method = "LRT")
  #   m3 <- mixed(value ~ treatment * phase + (1|id), obk.long, method = "PB")
  #   save(m1, m2, m3, file = "lmm_old_object.rda")
  #   dev_mode()
  load("lmm_old_object.rda") # 
  # load("tests/testthat/lmm_old_object.rda")
  expect_is(suppressWarnings(nice(m1)), "data.frame")
  expect_is(suppressWarnings(nice(m2)), "data.frame")
  expect_is(suppressWarnings(nice(m3)), "data.frame")
  expect_output(suppressWarnings(print(m1)), "treatment")
  expect_output(suppressWarnings(print(m2)), "treatment")
  expect_output(suppressWarnings(print(m3)), "treatment")
})


test_that("nice.mixed, print.mixed, and anova.mixed can handle objects with full.models", {
  load("mixed_with_dot.rda") # 
  #load("tests/testthat/mixed_with_dot.rda")
  expect_is(suppressWarnings(nice(sk_m1)), "data.frame")
  expect_is(suppressWarnings(nice(sk_m2)), "data.frame")
  expect_is(suppressWarnings(nice(t2)), "data.frame")
  expect_output(suppressWarnings(print(sk_m1)), "instruction")
  expect_output(suppressWarnings(print(sk_m2)), "instruction")
  expect_output(suppressWarnings(print(t2)), "treatment")
  expect_is(anova(sk_m1), "data.frame")
  expect_is(anova(sk_m2), "data.frame")
  expect_is(anova(t2), "data.frame")
})

test_that("lmer_alt works with GLMMs", {
  skip_if_not_installed("mlmRev")
  if (require("mlmRev")) {
    expect_that(lmer_alt(use ~ age*urban + (1 | district), family = binomial, data = Contraception, progress=FALSE), is_a("glmerMod"))
  }
})

test_that("lmer_alt works with NA in independent variables", {
  data(sk2011.2)

  # use only affirmation problems (S&K also splitted the data like this)
  sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])
  sk2_aff$instruction[5] <- NA
  assign("sk2_aff", sk2_aff, envir = .GlobalEnv)
  
  # set up model with maximal by-participant random slopes 
  sk_m1 <- suppressWarnings(lmer_alt(response ~ instruction*inference*type+(inference*type||id), sk2_aff, expand_re = TRUE))
  expect_true(inherits(sk_m1, "merModLmerTest") || inherits(sk_m1, "lmerModLmerTest"))
})

test_that("lmer_alt works with custom contrasts", {
  ## see: https://afex.singmann.science/forums/topic/trouble-with-ordered-contrasts-and-lmer_alt
  Subj <- rep(1:10, each = 10)
  Item <- rep(1:10, times = 10)
  IV1 <- rep(1:5, times = 20)
  DV <- rnorm(100)
  
  data <- as.data.frame(cbind(Subj, Item, IV1, DV))
  
  data$Subj <- as.factor(data$Subj)
  data$Item <- as.factor(data$Item)
  data$IV1 <- as.factor(data$IV1)
  
  
  contrasts(data$IV1) <- MASS::contr.sdif(5)
  
  mafex <- lmer_alt(DV ~ IV1 + (1 + IV1||Subj) + (1|Item), data = data)
  expect_is(mafex, "merMod")
  expect_identical(colnames(ranef(mafex)$Subj), 
                   c("(Intercept)", "re1.IV12.1", "re1.IV13.2", "re1.IV14.3", 
                     "re1.IV15.4"))
})

