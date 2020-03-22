
##########################
## 1: Specifying ANOVAs ##
##########################

# Example using a purely within-subjects design 
# (Maxwell & Delaney, 2004, Chapter 12, Table 12.5, p. 578):
data(md_12.1)
aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), 
       anova_table=list(correction = "none", es = "none"))

# Default output
aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))       


# examples using obk.long (see ?obk.long), a long version of the OBrienKaiser dataset (car package).
# Data is a split-plot or mixed design: contains both within- and between-subjects factors.
data(obk.long, package = "afex")

# estimate mixed ANOVA on the full design:
aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
        data = obk.long, observed = "gender")

aov_4(value ~ treatment * gender + (phase*hour|id), 
        data = obk.long, observed = "gender")

aov_ez("id", "value", obk.long, between = c("treatment", "gender"), 
        within = c("phase", "hour"), observed = "gender")

# the three calls return the same ANOVA table:
# Anova Table (Type 3 tests)
# 
# Response: value
#                         Effect          df   MSE         F  ges p.value
# 1                    treatment       2, 10 22.81    3.94 + .198    .055
# 2                       gender       1, 10 22.81    3.66 + .115    .085
# 3             treatment:gender       2, 10 22.81      2.86 .179    .104
# 4                        phase 1.60, 15.99  5.02 16.13 *** .151   <.001
# 5              treatment:phase 3.20, 15.99  5.02    4.85 * .097    .013
# 6                 gender:phase 1.60, 15.99  5.02      0.28 .003    .709
# 7       treatment:gender:phase 3.20, 15.99  5.02      0.64 .014    .612
# 8                         hour 1.84, 18.41  3.39 16.69 *** .125   <.001
# 9               treatment:hour 3.68, 18.41  3.39      0.09 .002    .979
# 10                 gender:hour 1.84, 18.41  3.39      0.45 .004    .628
# 11       treatment:gender:hour 3.68, 18.41  3.39      0.62 .011    .641
# 12                  phase:hour 3.60, 35.96  2.67      1.18 .015    .335
# 13        treatment:phase:hour 7.19, 35.96  2.67      0.35 .009    .930
# 14           gender:phase:hour 3.60, 35.96  2.67      0.93 .012    .449
# 15 treatment:gender:phase:hour 7.19, 35.96  2.67      0.74 .019    .646
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1
# 
# Sphericity correction method: GG 

# "numeric" variables are per default converted to factors (as long as factorize = TRUE):
obk.long$hour2 <- as.numeric(as.character(obk.long$hour))

# gives same results as calls before
aov_car(value ~ treatment * gender + Error(id/phase*hour2), 
        data = obk.long, observed = c("gender"))


# ANCOVA: adding a covariate (necessary to set factorize = FALSE)
aov_car(value ~ treatment * gender + age + Error(id/(phase*hour)), 
        data = obk.long, observed = c("gender", "age"), factorize = FALSE)

aov_4(value ~ treatment * gender + age + (phase*hour|id), 
        data = obk.long, observed = c("gender", "age"), factorize = FALSE)

aov_ez("id", "value", obk.long, between = c("treatment", "gender"), 
        within = c("phase", "hour"), covariate = "age", 
        observed = c("gender", "age"), factorize = FALSE)


# aggregating over one within-subjects factor (phase), with warning:
aov_car(value ~ treatment * gender + Error(id/hour), data = obk.long, observed = "gender")

aov_ez("id", "value", obk.long, c("treatment", "gender"), "hour", observed = "gender")

# aggregating over both within-subjects factors (again with warning),
# only between-subjects factors:
aov_car(value ~ treatment * gender + Error(id), data = obk.long, observed = c("gender"))
aov_4(value ~ treatment * gender + (1|id), data = obk.long, observed = c("gender"))
aov_ez("id", "value", obk.long, between = c("treatment", "gender"), observed = "gender")

# only within-subject factors (ignoring between-subjects factors)
aov_car(value ~ Error(id/(phase*hour)), data = obk.long)
aov_4(value ~ (phase*hour|id), data = obk.long)
aov_ez("id", "value", obk.long, within = c("phase", "hour"))

### changing defaults of ANOVA table:

# no df-correction & partial eta-squared:
aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
        data = obk.long, anova_table = list(correction = "none", es = "pes"))

# no df-correction and no MSE
aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
        data = obk.long,observed = "gender", 
        anova_table = list(correction = "none", MSE = FALSE))

# add p-value adjustment for all effects (see Cramer et al., 2015, PB&R)
aov_ez("id", "value", obk.long, between = "treatment", 
       within = c("phase", "hour"), 
       anova_table = list(p_adjust_method = "holm"))


###########################
## 2: Follow-up Analysis ##
###########################

# use data as above
data(obk.long, package = "afex")

# 1. obtain afex_aov object:
a1 <- aov_ez("id", "value", obk.long, between = c("treatment", "gender"), 
        within = c("phase", "hour"), observed = "gender")


# 1b. plot data using afex_plot function, for more see: 
## vignette("afex_plot_introduction", package = "afex")

## default plot uses univariate model-based CIs
afex_plot(a1, "hour", "gender", c("treatment", "phase"))

## you can use multivairate model and CIs
afex_plot(a1, "hour", "gender", c("treatment", "phase"), 
          emmeans_arg = list(model = "multivariate"))

## in a mixed between-within designs, no error-bars might be preferrable:
afex_plot(a1, "hour", "gender", c("treatment", "phase"), error = "none")

library("emmeans")  # package emmeans needs to be attached for follow-up tests.

# 2. obtain reference grid object (default uses univariate model):
r1 <- emmeans(a1, ~treatment +phase)
r1

# multivariate model may be more appropriate
r1 <- emmeans(a1, ~treatment +phase, model = "multivariate")
r1


# 3. create list of contrasts on the reference grid:
c1 <- list(
  A_B_pre = c(rep(0, 6), 0, -1, 1),  # A versus B for pretest
  A_B_comb = c(-0.5, 0.5, 0, -0.5, 0.5, 0, 0, 0, 0), # A vs. B for post and follow-up combined
  effect_post = c(0, 0, 0, -1, 0.5, 0.5, 0, 0, 0), # control versus A&B post
  effect_fup = c(-1, 0.5, 0.5, 0, 0, 0, 0, 0, 0), # control versus A&B follow-up
  effect_comb = c(-0.5, 0.25, 0.25, -0.5, 0.25, 0.25, 0, 0, 0) # control versus A&B combined
)

# 4. test contrasts on reference grid:
contrast(r1, c1)

# same as before, but using Bonferroni-Holm correction for multiple testing:
contrast(r1, c1, adjust = "holm")

# 2. (alternative): all pairwise comparisons of treatment:
emmeans(a1, "treatment", contr = "pairwise", model = "multivariate")

## set multivariate models globally:
# afex_options(emmeans_model = "multivariate")

#######################
## 3: Other examples ##
#######################
data(obk.long, package = "afex")

# replicating ?Anova using aov_car:
obk_anova <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
        data = obk.long, type = 2)
# in contrast to aov you do not need the within-subject factors outside Error()

str(obk_anova, 1, give.attr = FALSE)
# List of 5
#  $ anova_table:Classes 'anova' and 'data.frame':	15 obs. of  6 variables:
#  $ aov        :List of 5
#  $ Anova      :List of 14
#  $ lm         :List of 13
#  $ data       :List of 3

obk_anova$Anova
# Type II Repeated Measures MANOVA Tests: Pillai test statistic
#                             Df test stat approx F num Df den Df    Pr(>F)    
# (Intercept)                  1   0.96954   318.34      1     10 6.532e-09 ***
# treatment                    2   0.48092     4.63      2     10 0.0376868 *  
# gender                       1   0.20356     2.56      1     10 0.1409735    
# treatment:gender             2   0.36350     2.86      2     10 0.1044692    
# phase                        1   0.85052    25.61      2      9 0.0001930 ***
# treatment:phase              2   0.68518     2.61      4     20 0.0667354 .  
# gender:phase                 1   0.04314     0.20      2      9 0.8199968    
# treatment:gender:phase       2   0.31060     0.92      4     20 0.4721498    
# hour                         1   0.93468    25.04      4      7 0.0003043 ***
# treatment:hour               2   0.30144     0.35      8     16 0.9295212    
# gender:hour                  1   0.29274     0.72      4      7 0.6023742    
# treatment:gender:hour        2   0.57022     0.80      8     16 0.6131884    
# phase:hour                   1   0.54958     0.46      8      3 0.8324517    
# treatment:phase:hour         2   0.66367     0.25     16      8 0.9914415    
# gender:phase:hour            1   0.69505     0.85      8      3 0.6202076    
# treatment:gender:phase:hour  2   0.79277     0.33     16      8 0.9723693    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

