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

# Recreate Figure 4 (corrected version):

sk2_aff <- droplevels(sk2_final[sk2_final$what == "affirmation",])
sk2_aff$type2 <- factor(sk2_aff$inference:sk2_aff$type, levels = c("MP:prological", 
                            "MP:neutral", "MP:counterlogical", "AC:counterlogical", 
                            "AC:neutral", "AC:prological"))
a1_b <- aov_ez("id", "response", sk2_aff, 
       between = "instruction", within = c("type2"))

sk2_den <- droplevels(sk2_final[sk2_final$what == "denial",])
sk2_den$type2 <- factor(sk2_den$inference:sk2_den$type, levels = c("MT:prological", 
                            "MT:neutral", "MT:counterlogical", "DA:counterlogical", 
                            "DA:neutral","DA:prological"))
a2_b <- aov_ez("id", "response", sk2_den, 
       between = "instruction", within = c("type2"))

if (requireNamespace("emmeans") && requireNamespace("ggplot2")) {
  afex_plot(a1_b,"type2", "instruction") + 
    ggplot2::coord_cartesian(ylim = c(0, 100))
  afex_plot(a2_b,"type2", "instruction") + 
    ggplot2::coord_cartesian(ylim = c(0, 100))
}
