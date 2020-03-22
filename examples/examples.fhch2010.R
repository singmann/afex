
data("fhch2010")
str(fhch2010)

a1 <- aov_ez("id", "log_rt", fhch2010, between = "task", 
             within = c("density", "frequency", "length", "stimulus"))
nice(a1)

if (requireNamespace("emmeans") && requireNamespace("ggplot2")) {
  afex_plot(a1, "length", "frequency", c("task", "stimulus"), error = "within")
  
  afex_plot(a1, "density", "frequency", c("task", "stimulus"), error = "within")
}


\dontrun{
a2 <- aov_ez("id", "rt", fhch2010, between = "task", 
             within = c("density", "frequency", "length", "stimulus"))
nice(a2)

if (requireNamespace("emmeans") && requireNamespace("ggplot2")) {
  afex_plot(a2, "length", "frequency", c("task", "stimulus"), error = "within")
  
  afex_plot(a2, "density", "frequency", c("task", "stimulus"), error = "within")
}
}
