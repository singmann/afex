
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
##                         Effect          df   MSE         F  ges p.value
## 1                    treatment       2, 10 22.81    3.94 +  .20     .05
## 2                       gender       1, 10 22.81    3.66 +  .11     .08
## 3             treatment:gender       2, 10 22.81      2.86  .18     .10
## 4                        phase 1.60, 15.99  5.02 16.13 ***  .15   .0003
## 5              treatment:phase 3.20, 15.99  5.02    4.85 *  .10     .01
## 6                 gender:phase 1.60, 15.99  5.02      0.28 .003     .71
## 7       treatment:gender:phase 3.20, 15.99  5.02      0.64  .01     .61
## 8                         hour 1.84, 18.41  3.39 16.69 ***  .13  <.0001
## 9               treatment:hour 3.68, 18.41  3.39      0.09 .002     .98
## 10                 gender:hour 1.84, 18.41  3.39      0.45 .004     .63
## 11       treatment:gender:hour 3.68, 18.41  3.39      0.62  .01     .64
## 12                  phase:hour 3.60, 35.96  2.67      1.18  .02     .33
## 13        treatment:phase:hour 7.19, 35.96  2.67      0.35 .009     .93
## 14           gender:phase:hour 3.60, 35.96  2.67      0.93  .01     .45
## 15 treatment:gender:phase:hour 7.19, 35.96  2.67      0.74  .02     .65


# "numeric" variables are per default converted to factors (as long as factorize = TRUE):
obk.long$hour2 <- as.numeric(as.character(obk.long$hour))

# gives same results as calls before
aov_car(value ~ treatment * gender + Error(id/hour2*phase), 
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


###########################
## 2: Follow-up Analysis ##
###########################

# use data as above
data(obk.long, package = "afex")

# 1. obtain afex_aov object:
a1 <- aov_ez("id", "value", obk.long, between = c("treatment", "gender"), 
        within = c("phase", "hour"), observed = "gender")

# 1b. plot data:
lsmip(a1, gender ~ hour | treatment+phase)

# 2. obtain reference grid object:
r1 <- lsmeans(a1, ~treatment +phase)
r1

# 3. create list of contrasts on the reference grid:
c1 <- list(
  A_B_pre = c(0, -1, 1, rep(0, 6)),  # A versus B for pretest
  A_B_comb = c(0, 0, 0, 0, -0.5, 0.5, 0, -0.5, 0.5), # A vs. B for post and follow-up combined
  effect_post = c(0, 0, 0, -1, 0.5, 0.5, 0, 0, 0), # control versus A&B post
  effect_fup = c(0, 0, 0, 0, 0, 0, -1, 0.5, 0.5), # control versus A&B follow-up
  effect_comb = c(0, 0, 0, -0.5, 0.25, 0.25, -0.5, 0.25, 0.25) # control versus A&B combined
)

# 4. test contrasts on reference grid:
contrast(r1, c1)

# same as before, but using Bonferroni-Holm correction for multiple testing:
contrast(r1, c1, adjust = "holm")

# 2. (alternative): all pairwise comparisons of treatment:
lsmeans(a1, "treatment", contr = "pairwise")

#######################
## 3: Other examples ##
#######################
data(obk.long, package = "afex")

# replicating ?Anova using aov_car:
obk_anova <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
        data = obk.long, type = 2)
# in contrast to aov you do not need the within-subject factors outside Error()

str(obk_anova, 1, give.attr = FALSE)
## List of 6
##  $ anova_table:Classes 'anova' and 'data.frame':  15 obs. of  6 variables:
##  $ aov        :List of 5
##  $ Anova      :List of 14
##  $ lm         :List of 13
##  $ data       :List of 3
##  $ information:List of 5

obk_anova$Anova
## Type II Repeated Measures MANOVA Tests: Pillai test statistic
##                             Df test stat approx F num Df den Df       Pr(>F)    
## (Intercept)                  1     0.970      318      1     10 0.0000000065 ***
## treatment                    2     0.481        5      2     10      0.03769 *  
## gender                       1     0.204        3      1     10      0.14097    
## treatment:gender             2     0.364        3      2     10      0.10447    
## phase                        1     0.851       26      2      9      0.00019 ***
## treatment:phase              2     0.685        3      4     20      0.06674 .  
## gender:phase                 1     0.043        0      2      9      0.82000    
## treatment:gender:phase       2     0.311        1      4     20      0.47215    
## hour                         1     0.935       25      4      7      0.00030 ***
## treatment:hour               2     0.301        0      8     16      0.92952    
## gender:hour                  1     0.293        1      4      7      0.60237    
## treatment:gender:hour        2     0.570        1      8     16      0.61319    
## phase:hour                   1     0.550        0      8      3      0.83245    
## treatment:phase:hour         2     0.664        0     16      8      0.99144    
## gender:phase:hour            1     0.695        1      8      3      0.62021    
## treatment:gender:phase:hour  2     0.793        0     16      8      0.97237    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

