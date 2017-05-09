[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/afex)](http://cran.r-project.org/package=afex)
[![monthly downloads](http://cranlogs.r-pkg.org/badges/afex)](http://cranlogs.r-pkg.org/badges/afex)
[![total downloads](http://cranlogs.r-pkg.org/badges/grand-total/afex)](http://cranlogs.r-pkg.org/badges/grand-total/afex)
[![Research software impact](http://depsy.org/api/package/cran/afex/badge.svg)](http://depsy.org/package/r/afex)
[![Travis-CI Build Status](https://travis-ci.org/singmann/afex.svg?branch=master)](https://travis-ci.org/singmann/afex)


afex: Analysis of Factorial EXperiments
====

The two main functionalities provided by `afex` are:

1. A coherent and intuitive interface to run standard ANOVAs with any number of within- or between-subjects variables (the relevant functions are now called `aov_car`, `aov_ez`, and `aov_4`).
2. Fit mixed models (via `lme4` `lmer` or `glmer`) and obtain *p*-values for fixed effects in one call to function `mixed`. 

The default outputs of those functions can be directly passed to `lsmeans` for post-hoc/follow-up tests or contrasts. 

For `afex` support visit: [afex.singmann.science](http://afex.singmann.science/)


## Installation

- `afex` is available from CRAN so the current stable version can be installed directly via: 
  `install.packages("afex")`

- To install the latest development version you will need the [`devtools`](https://github.com/hadley/devtools) package: 
  `devtools::install_github("singmann/afex@master")`

## ANOVA functionality

To calculate an ANOVA, `afex` requires the data to be in the long format (i.e., one row per data point/observation). An ANOVA can then be calculated via one of three functions that only differ in the way how to specify the ANOVA:

- In `aov_ez` the columns containing dependent variable, id variable, and factors need to be specified as character vectors.
- `aov_car` behaves similar to standard `aov` and requires the ANOVA to be specified as a formula containing an `Error` term (at least to identify the id variable).
- `aov_4` allows the ANOVA to be specified via a formula similar to `lme4::lmer` (with one random effects term).

A further overview is provided by the [vignette](https://cran.rstudio.com/web/packages/afex/vignettes/afex_anova_example.html). The following code provides a simple example for an ANOVA with both between- and within-subject factors, see also `?aov_car` in `R`.

```
require(afex)
# examples data set with both within- and between-subjects factors (see ?obk.long)
data(obk.long, package = "afex")
head(obk.long)  # data in long format
#   id treatment gender   age phase hour value
# 1  1   control      M -4.75   pre    1     1
# 2  1   control      M -4.75   pre    2     2
# 3  1   control      M -4.75   pre    3     4
# 4  1   control      M -4.75   pre    4     2
# 5  1   control      M -4.75   pre    5     1
# 6  1   control      M -4.75  post    1     3

# estimate mixed ANOVA on the full design:
aov_ez("id", "value", obk.long, between = c("treatment", "gender"), 
        within = c("phase", "hour"), observed = "gender")

aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
        data = obk.long, observed = "gender")

aov_4(value ~ treatment * gender + (phase*hour|id), 
        data = obk.long, observed = "gender")

# the three calls return the same ANOVA table:
# Anova Table (Type 3 tests)
# 
# Response: value
#                         Effect          df   MSE         F  ges p.value
# 1                    treatment       2, 10 22.81    3.94 +  .20     .05
# 2                       gender       1, 10 22.81    3.66 +  .11     .08
# 3             treatment:gender       2, 10 22.81      2.86  .18     .10
# 4                        phase 1.60, 15.99  5.02 16.13 ***  .15   .0003
# 5              treatment:phase 3.20, 15.99  5.02    4.85 *  .10     .01
# 6                 gender:phase 1.60, 15.99  5.02      0.28 .003     .71
# 7       treatment:gender:phase 3.20, 15.99  5.02      0.64  .01     .61
# 8                         hour 1.84, 18.41  3.39 16.69 ***  .13  <.0001
# 9               treatment:hour 3.68, 18.41  3.39      0.09 .002     .98
# 10                 gender:hour 1.84, 18.41  3.39      0.45 .004     .63
# 11       treatment:gender:hour 3.68, 18.41  3.39      0.62  .01     .64
# 12                  phase:hour 3.60, 35.96  2.67      1.18  .02     .33
# 13        treatment:phase:hour 7.19, 35.96  2.67      0.35 .009     .93
# 14           gender:phase:hour 3.60, 35.96  2.67      0.93  .01     .45
# 15 treatment:gender:phase:hour 7.19, 35.96  2.67      0.74  .02     .65
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1
# 
# Sphericity correction method: GG 
```

## Mixed Models

Function `mixed()` fits a mixed model with `lme4::lmer` (or `lme4::glmer` if a `family` argument is passed) and then calculates *p*-values for fixed effects using a variety of methods. The formula to `mixed` needs to be the same as in a call to `lme4::lmer`. The default method for calculation of *p*-values is `'KR'` (Kenward-Roger) which only works for linear mixed models (i.e., no `family` argument) and can require considerable RAM and time, but provides the best control of Type I errors. Other methods are `'S'` (Satterthwaite, similar to `'KR'` but requires less RAM), `'PB'` (parametric bootstrap), and `'LRT'` (likelihood-ratio test). 

More examples are provided in the [vignette](https://cran.rstudio.com/web/packages/afex/vignettes/afex_mixed_example.html), the following is a short example using some published data (see also `?mixed`).

```
require(afex)
data("sk2011.2")

# use only affirmation problems (S&K also splitted the data like this)
sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])

# set up model with maximal by-participant random slopes 
sk_m1 <- mixed(response ~ instruction*inference*type+(inference*type|id), sk2_aff)

sk_m1 # prints ANOVA table with nicely rounded numbers (i.e., as characters)
# Mixed Model Anova Table (Type 3 tests, KR-method)
# 
# Model: response ~ instruction * inference * type + (inference * type | 
# Model:     id)
# Data: sk2_aff
#                       Effect    df         F p.value
# 1                instruction 1, 61      0.11     .75
# 2                  inference 1, 61 80.25 ***  <.0001
# 3                       type 2, 60 23.01 ***  <.0001
# 4      instruction:inference 1, 61 15.03 ***   .0003
# 5           instruction:type 2, 60    2.90 +     .06
# 6             inference:type 2, 60 47.10 ***  <.0001
# 7 instruction:inference:type 2, 60    3.70 *     .03
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

nice(sk_m1)  # returns the same but without printing potential warnings
# Mixed Model Anova Table (Type 3 tests, KR-method)
# 
# Model: response ~ instruction * inference * type + (inference * type | 
# Model:     id)
# Data: sk2_aff
#                       Effect    df         F p.value
# 1                instruction 1, 61      0.11     .75
# 2                  inference 1, 61 80.25 ***  <.0001
# 3                       type 2, 60 23.01 ***  <.0001
# 4      instruction:inference 1, 61 15.03 ***   .0003
# 5           instruction:type 2, 60    2.90 +     .06
# 6             inference:type 2, 60 47.10 ***  <.0001
# 7 instruction:inference:type 2, 60    3.70 *     .03
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

anova(sk_m1) # returns and prints numeric ANOVA table (i.e., not-rounded)
# Mixed Model Anova Table (Type 3 tests, KR-method)
# 
# Model: response ~ instruction * inference * type + (inference * type | 
# Model:     id)
# Data: sk2_aff
#                            num Df den Df       F    Pr(>F)    
# instruction                     1     61  0.1052 0.7467480    
# inference                       1     61 80.2495 1.007e-12 ***
# type                            2     60 23.0054 3.837e-08 ***
# instruction:inference           1     61 15.0318 0.0002613 ***
# instruction:type                2     60  2.9026 0.0626245 .  
# inference:type                  2     60 47.1011 5.033e-13 ***
# instruction:inference:type      2     60  3.6972 0.0306076 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(sk_m1) # lmer summary of full model
# Linear mixed model fit by REML ['merModLmerTest']
# Formula: response ~ instruction * inference * type + (inference * type |      id)
#    Data: sk2_aff
# 
# REML criterion at convergence: 10393.7
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.9241 -0.3314  0.0654  0.3796  3.7675 
# 
# Random effects:
#  Groups   Name             Variance Std.Dev. Corr                         
#  id       (Intercept)      141.84   11.910                                
#           inference1       158.83   12.603   -0.71                        
#           type1             39.81    6.310   -0.24 -0.09                  
#           type2             10.43    3.230    0.43  0.23 -0.41            
#           inference1:type1  54.38    7.374   -0.05 -0.33 -0.55 -0.42      
#           inference1:type2   4.33    2.081   -0.39  0.50  0.16 -0.31 -0.31
#  Residual                  443.39   21.057                                
# Number of obs: 1134, groups:  id, 63
# 
# Fixed effects:
#                               Estimate Std. Error t value
# (Intercept)                    73.0586     1.6257   44.94
# instruction1                   -0.5274     1.6257   -0.32
# inference1                     15.2891     1.7067    8.96
# type1                          -1.1946     1.1892   -1.00
# type2                          -5.4376     0.9736   -5.59
# instruction1:inference1         6.6171     1.7067    3.88
# instruction1:type1             -2.0763     1.1892   -1.75
# instruction1:type2              2.2345     0.9736    2.30
# inference1:type1               10.2455     1.2828    7.99
# inference1:type2                1.8956     0.9225    2.05
# instruction1:inference1:type1  -3.4642     1.2828   -2.70
# instruction1:inference1:type2   0.5784     0.9225    0.63
# [...]
```

----
Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
