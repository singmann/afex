context("mixed interplay with effects")


test_that("mixed works with effects", {
  data("Machines", package = "MEMSS") 
  # simple model with random-slopes for repeated-measures factor
  # requires: set_data_arg = TRUE
  m1 <- mixed(score ~ Machine + (Machine|Worker), data=Machines, 
            set_data_arg = TRUE, progress = FALSE)
  
  testthat::skip_if_not_installed("effects")
  testthat::skip_if_not_installed("emmeans") 
  library("effects")
  
  set_default_contrasts()

  em1 <- emmeans::emmeans(m1, "Machine")
  ef1 <- Effect("Machine", m1$full_model) #
  
  expect_false(any(
    as.data.frame(ef1)$fit == as.data.frame(em1)$emmean
  ))
  
  set_sum_contrasts()
  ef2 <- Effect("Machine", m1$full_model)

  expect_equal(as.data.frame(ef2)$fit, as.data.frame(em1)$emmean)
  
})
