
data("fhch2010")
str(fhch2010)

a1 <- aov_ez("id", "log_rt", fhch2010, between = "task", 
             within = c("density", "frequency", "length", "stimulus"))
nice(a1)

lsmip(a1, frequency~length|task+stimulus)

lsmip(a1, frequency~density|task+stimulus)

\dontrun{
a2 <- aov_ez("id", "rt", fhch2010, between = "task", 
             within = c("density", "frequency", "length", "stimulus"))
nice(a2)

lsmip(a2, frequency~length|task+stimulus)

lsmip(a2, frequency~density|task+stimulus)
}
