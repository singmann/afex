

context("mixed: known bugs")

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
  load("lmm_old_object.rda")
  expect_is(suppressWarnings(nice(m1)), "data.frame")
  expect_is(suppressWarnings(nice(m2)), "data.frame")
  expect_is(suppressWarnings(nice(m3)), "data.frame")
  expect_output(suppressWarnings(print(m1)), "treatment")
  expect_output(suppressWarnings(print(m2)), "treatment")
  expect_output(suppressWarnings(print(m3)), "treatment")
})
