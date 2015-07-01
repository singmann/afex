data("sk2011.2")

## remove excluded participants:

sk2_final <- droplevels(sk2011.2[!(sk2011.2$id %in% c(7, 8, 9, 12, 16, 17, 24, 30)),])
str(sk2_final)

## Table 2 (inference = problem):
aov_ez("id", "response", sk2_final[sk2_final$what == "affirmation",], 
       between = "instruction", within = c("inference", "type"),
       anova_table=list(es = "pes"))

aov_ez("id", "response", sk2_final[sk2_final$what == "denial",], 
       between = "instruction", within = c("inference", "type"),
       anova_table=list(es = "pes"))
