
## example from Olejnik & Algina (2003)
# "Repeated Measures Design" (pp. 439):
data(md_12.1)
# create object of class afex_aov:
rmd <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))
nice(rmd)
# use different es:
nice(rmd, es = "pes") # noise: .82
nice(rmd, es = "ges") # noise: .39

# same data other approach:
rmd2 <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), 
               anova_table=list(correction = "none", es = "none"))
nice(rmd2)
nice(rmd2, correction = "GG")
nice(rmd2, correction = "GG", es = "ges")

# exampel using obk.long (see ?obk.long), a long version of the OBrienKaiser dataset from car.
data(obk.long)
# create object of class afex_aov:
tmp.aov <- aov_car(value ~ treatment * gender + Error(id/phase*hour), data = obk.long)

nice(tmp.aov, observed = "gender")

nice(tmp.aov, observed = "gender", sig.symbols = rep("", 4))

\dontrun{
# use package ascii or xtable for formatting of tables ready for printing.

full <- nice(tmp.aov, observed = "gender")

require(ascii)
print(ascii(full, include.rownames = FALSE, caption = "ANOVA 1"), type = "org")

require(xtable)
print.xtable(xtable(full, caption = "ANOVA 2"), include.rownames = FALSE)
}
