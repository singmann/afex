

context("mixed: known bugs")

test_that("mixed works with long formulas", {
  data(obk.long)
  obk2 <- obk.long
  colnames(obk2) <- sapply(colnames(obk2), function(x) paste0(x, x, x, x, x, x))
  expect_is(mixed(valuevaluevaluevaluevaluevalue ~ treatmenttreatmenttreatmenttreatmenttreatmenttreatment * phasephasephasephasephasephase * hourhourhourhourhourhour + (1|idididididid), obk2, method = "LRT", progress = FALSE), "mixed")
})

