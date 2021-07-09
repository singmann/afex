
##################################
## Simple Examples (from MEMSS) ##
##################################

data("Machines", package = "MEMSS") 

# simple model with random-slopes for repeated-measures factor
m1 <- mixed(score ~ Machine + (Machine|Worker), data=Machines)
m1

# suppress correlations among random effect parameters with || and expand_re = TRUE
m2 <- mixed(score ~ Machine + (Machine||Worker), data=Machines, expand_re = TRUE)
m2

## compare:
summary(m1)$varcor
summary(m2)$varcor
# for wrong solution see: 
# summary(lmer(score ~ Machine + (Machine||Worker), data=Machines))$varcor

if (requireNamespace("emmeans")) {
# follow-up tests
library("emmeans")  # package emmeans needs to be attached for follow-up tests.
(emm1 <- emmeans(m1, "Machine"))
pairs(emm1, adjust = "holm") # all pairwise comparisons
con1 <- list(
  c1 = c(1, -0.5, -0.5), # 1 versus other 2
  c2 = c(0.5, -1, 0.5) # 1 and 3 versus  2
)
contrast(emm1, con1, adjust = "holm")

if (requireNamespace("ggplot2")) {
# plotting 
afex_plot(m1, "Machine") ## default uses model-based CIs
## within-subjects CIs somewhat more in line with pairwirse comparisons:
afex_plot(m1, "Machine", error = "within") 

## less differences between CIs for model without correlations:
afex_plot(m2, "Machine")
afex_plot(m2, "Machine", error = "within")
}}

\dontrun{
#######################
### Further Options ###
#######################

## Multicore:

require(parallel)
(nc <- detectCores()) # number of cores
cl <- makeCluster(rep("localhost", nc)) # make cluster
# to keep track of what the function is doindg redirect output to outfile:
# cl <- makeCluster(rep("localhost", nc), outfile = "cl.log.txt")

data("Machines", package = "MEMSS") 
## There are two ways to use multicore:

# 1. Obtain fits with multicore (e.g. for likelihood ratio tests, LRT):
mixed(score ~ Machine + (Machine|Worker), data=Machines, cl = cl, 
      method = "LRT")

# 2. Obtain PB samples via multicore: 
mixed(score ~ Machine + (Machine|Worker), data=Machines,
 method = "PB", args_test = list(nsim = 50, cl = cl)) # better use 500 or 1000 

## Both ways can be combined:
# 2. Obtain PB samples via multicore: 
mixed(score ~ Machine + (Machine|Worker), data=Machines, cl = cl,
 method = "PB", args_test = list(nsim = 50, cl = cl))

#### use all_fit = TRUE and expand_re = TRUE:
data("sk2011.2") # data described in more detail below
sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])

require(optimx) # uses two more algorithms
sk2_aff_b <- mixed(response ~ instruction*type+(inference*type||id), sk2_aff,
               expand_re = TRUE, all_fit = TRUE, method = "LRT")
attr(sk2_aff_b, "all_fit_selected")
attr(sk2_aff_b, "all_fit_logLik")

# considerably faster with multicore:
clusterEvalQ(cl, library(optimx)) # need to load optimx in cluster
sk2_aff_b2 <- mixed(response ~ instruction*type+(inference*type||id), sk2_aff,
               expand_re = TRUE, all_fit = TRUE, cl=cl, method = "LRT")
attr(sk2_aff_b2, "all_fit_selected")
attr(sk2_aff_b2, "all_fit_logLik")


stopCluster(cl)

}

###################################################
## Replicating Maxwell & Delaney (2004) Examples ##
###################################################
\dontrun{

### replicate results from Table 15.4 (Maxwell & Delaney, 2004, p. 789)
data(md_15.1)
# random intercept plus random slope
(t15.4a <- mixed(iq ~ timecat + (1+time|id),data=md_15.1))

# to also replicate exact parameters use treatment.contrasts and the last level as base level:
contrasts(md_15.1$timecat) <- contr.treatment(4, base = 4)
(t15.4b <- mixed(iq ~ timecat + (1+time|id),data=md_15.1, check_contrasts=FALSE))
summary(t15.4a)  # gives "wrong" parameters extimates
summary(t15.4b)  # identical parameters estimates

# for more examples from chapter 15 see ?md_15.1

### replicate results from Table 16.3 (Maxwell & Delaney, 2004, p. 837)
data(md_16.1)

# original results need treatment contrasts:
(mixed1_orig <- mixed(severity ~ sex + (1|id), md_16.1, check_contrasts=FALSE))
summary(mixed1_orig$full_model)

# p-value stays the same with afex default contrasts (contr.sum),
# but estimates and t-values for the fixed effects parameters change.
(mixed1 <- mixed(severity ~ sex + (1|id), md_16.1))
summary(mixed1$full_model)


# data for next examples (Maxwell & Delaney, Table 16.4)
data(md_16.4)
str(md_16.4)

### replicate results from Table 16.6 (Maxwell & Delaney, 2004, p. 845)
# Note that (1|room:cond) is needed because room is nested within cond.
# p-value (almost) holds.
(mixed2 <- mixed(induct ~ cond + (1|room:cond), md_16.4))
# (differences are dut to the use of Kenward-Roger approximation here,
# whereas M&W's p-values are based on uncorrected df.)

# again, to obtain identical parameter and t-values, use treatment contrasts:
summary(mixed2) # not identical

# prepare new data.frame with contrasts:
md_16.4b <- within(md_16.4, cond <- C(cond, contr.treatment, base = 2))
str(md_16.4b)

# p-value stays identical:
(mixed2_orig <- mixed(induct ~ cond + (1|room:cond), md_16.4b, 
                      check_contrasts=FALSE))
summary(mixed2_orig$full_model) # replicates parameters


### replicate results from Table 16.7 (Maxwell & Delaney, 2004, p. 851)
# F-values (almost) hold, p-values (especially for skill) are off
(mixed3 <- mixed(induct ~ cond + skill + (1|room:cond), md_16.4))

# however, parameters are perfectly recovered when using the original contrasts:
mixed3_orig <- mixed(induct ~ cond + skill + (1|room:cond), md_16.4b, 
                     check_contrasts=FALSE)
summary(mixed3_orig)


### replicate results from Table 16.10 (Maxwell & Delaney, 2004, p. 862)
# for this we need to center cog:
md_16.4b$cog <- scale(md_16.4b$cog, scale=FALSE)

# F-values and p-values are relatively off:
(mixed4 <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b))
# contrast has a relatively important influence on cog
(mixed4_orig <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b, 
                      check_contrasts=FALSE))

# parameters are again almost perfectly recovered:
summary(mixed4_orig)
}

###########################
## Full Analysis Example ##
###########################

\dontrun{
### split-plot experiment (Singmann & Klauer, 2011, Exp. 2)
## between-factor: instruction
## within-factor: inference & type
## hypothesis: three-way interaction
data("sk2011.2")

# use only affirmation problems (S&K also splitted the data like this)
sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])

# set up model with maximal by-participant random slopes 
sk_m1 <- mixed(response ~ instruction*inference*type+(inference*type|id), sk2_aff)

sk_m1 # prints ANOVA table with nicely rounded numbers (i.e., as characters)
nice(sk_m1)  # returns the same but without printing potential warnings
anova(sk_m1) # returns and prints numeric ANOVA table (i.e., not-rounded)
summary(sk_m1) # lmer summary of full model

# same model but using Kenward-Roger approximation of df
# very similar results but slower
sk_m1b <- mixed(response ~ instruction*inference*type+(inference*type|id), 
                sk2_aff, method="KR")
nice(sk_m1b)
# identical results as:
anova(sk_m1$full_model)

# suppressing correlation among random slopes: very similar results, but
# significantly faster and often less convergence warnings.
sk_m2 <- mixed(response ~ instruction*inference*type+(inference*type||id), sk2_aff,
               expand_re = TRUE)
sk_m2

## mixed objects can be passed to emmeans
library("emmeans")  # however, package emmeans needs to be attached first

# emmeans also approximate df which takes time with default Kenward-Roger
emm_options(lmer.df = "Kenward-Roger") # default setting, slow
emm_options(lmer.df = "Satterthwaite") # faster setting, preferrable
emm_options(lmer.df = "asymptotic") # the fastest, df = infinity


# recreates basically Figure 4 (S&K, 2011, upper panel)
# only the 4th and 6th x-axis position are flipped
afex_plot(sk_m1, x = c("type", "inference"), trace = "instruction")

# set up reference grid for custom contrasts:
(rg1 <- emmeans(sk_m1, c("instruction", "type", "inference")))

# set up contrasts on reference grid:
contr_sk2 <- list(
  ded_validity_effect = c(rep(0, 4), 1, rep(0, 5), -1, 0),
  ind_validity_effect = c(rep(0, 5), 1, rep(0, 5), -1),
  counter_MP = c(rep(0, 4), 1, -1, rep(0, 6)),
  counter_AC = c(rep(0, 10), 1, -1)
)

# test the main double dissociation (see S&K, p. 268)
contrast(rg1, contr_sk2, adjust = "holm")
# all effects are significant.
}

####################
## Other Examples ##
####################

\dontrun{

# use the obk.long data (not reasonable, no random slopes)
data(obk.long)
mixed(value ~ treatment * phase + (1|id), obk.long)

# Examples for using the per.parameter argument 
# note, require method = "nested-KR", "LRT", or "PB" 
# also we use custom contrasts
data(obk.long, package = "afex")
obk.long$hour <- ordered(obk.long$hour)
contrasts(obk.long$phase) <- "contr.sum"
contrasts(obk.long$treatment) <- "contr.sum" 

# tests only the main effect parameters of hour individually per parameter.
mixed(value ~ treatment*phase*hour +(1|id), per_parameter = "^hour$", 
      data = obk.long, method = "nested-KR", check_contrasts = FALSE)

# tests all parameters including hour individually
mixed(value ~ treatment*phase*hour +(1|id), per_parameter = "hour", 
      data = obk.long, method = "nested-KR", check_contrasts = FALSE)

# tests all parameters individually
mixed(value ~ treatment*phase*hour +(1|id), per_parameter = ".", 
      data = obk.long, method = "nested-KR", check_contrasts = FALSE)

# example data from package languageR: Lexical decision latencies elicited from
# 21 subjects for 79 English concrete nouns, with variables linked to subject or
# word.
data(lexdec, package = "languageR")

# using the simplest model
m1 <- mixed(RT ~ Correct + Trial + PrevType * meanWeight + 
    Frequency + NativeLanguage * Length + (1|Subject) + (1|Word), data = lexdec)
m1
# Mixed Model Anova Table (Type 3 tests, S-method)
# 
# Model: RT ~ Correct + Trial + PrevType * meanWeight + Frequency + NativeLanguage * 
# Model:     Length + (1 | Subject) + (1 | Word)
# Data: lexdec
#                  Effect         df         F p.value
# 1               Correct 1, 1627.67   8.16 **    .004
# 2                 Trial 1, 1591.92   7.58 **    .006
# 3              PrevType 1, 1605.05      0.17    .680
# 4            meanWeight   1, 74.37 14.85 ***   <.001
# 5             Frequency   1, 75.06 56.54 ***   <.001
# 6        NativeLanguage   1, 27.12      0.70    .412
# 7                Length   1, 74.80   8.70 **    .004
# 8   PrevType:meanWeight 1, 1600.79    6.19 *    .013
# 9 NativeLanguage:Length 1, 1554.49 14.24 ***   <.001
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

# Fitting a GLMM using parametric bootstrap:
require("mlmRev") # for the data, see ?Contraception

gm1 <- mixed(use ~ age + I(age^2) + urban + livch + (1 | district), method = "PB",
 family = binomial, data = Contraception, args_test = list(nsim = 10))
## note that nsim = 10 is way too low for all real examples!

}

\dontrun{
#####################################
## Interplay with effects packages ##
#####################################

data("Machines", package = "MEMSS") 
# simple model with random-slopes for repeated-measures factor
m1 <- mixed(score ~ Machine + (Machine|Worker), data=Machines, 
            set_data_arg = TRUE) ## necessary for it to work!
  
library("effects")

Effect("Machine", m1$full_model) # not correct:
#  Machine effect
# Machine
#        A        B        C 
# 59.65000 52.35556 60.32222 

# compare:
emmeans::emmeans(m1, "Machine")
 # Machine   emmean       SE  df asymp.LCL asymp.UCL
 # A       52.35556 1.680711 Inf  49.06142  55.64969
 # B       60.32222 3.528546 Inf  53.40640  67.23804
 # C       66.27222 1.806273 Inf  62.73199  69.81245

## necessary to set contr.sum globally:
set_sum_contrasts()
Effect("Machine", m1$full_model)
#  Machine effect
# Machine
#        A        B        C 
# 52.35556 60.32222 66.27222 

plot(Effect("Machine", m1$full_model))
}
