
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

# same model but using Satterthwaite approximation of df, very similar results
sk_m1b <- mixed(response ~ instruction*inference*type+(inference*type|id), 
                sk2_aff, method="S")
nice(sk_m1b)
# identical results as:
lmerTest::anova(sk_m1$full_model)

# suppressing correlation among random slopes:
# very similar results, but significantly faster and often better convergence. 
sk_m2 <- mixed(response ~ instruction*inference*type+(inference*type||id), sk2_aff,
               expand_re = TRUE)
sk_m2

## mixed objects can be passed to lsmeans directly:

# recreates basically Figure 4 (S&K, 2011, upper panel)
# only the 4th and 6th x-axis position are flipped
lsmip(sk_m1, instruction~type+inference)

# set up reference grid for custom contrasts:
# this can be made faster via:
lsm.options(lmer.df = "Kenward-Roger") # set df for lsmeans to KR
# lsm.options(lmer.df = "Satterthwaite") # the default
# lsm.options(lmer.df = "asymptotic") # the fastest, no df
(rg1 <- lsmeans(sk_m1, c("instruction", "type", "inference")))

# set up contrasts on reference grid:
contr_sk2 <- list(
  ded_validity_effect = c(rep(0, 4), 1, rep(0, 5), -1, 0),
  ind_validity_effect = c(rep(0, 5), 1, rep(0, 5), -1),
  counter_MP = c(rep(0, 4), 1, -1, rep(0, 6)),
  counter_AC = c(rep(0, 10), 1, -1)
)

# test the main double dissociation (see S&K, p. 268)
contrast(rg1, contr_sk2, adjust = "holm")
# only plausibility effect is not significant here.
}

###################################################
## Replicating Maxwell & Delaney (2004) Examples ##
###################################################

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
(mixed2_orig <- mixed(induct ~ cond + (1|room:cond), md_16.4b, check_contrasts=FALSE))
summary(mixed2_orig$full_model) # replicates parameters


### replicate results from Table 16.7 (Maxwell & Delaney, 2004, p. 851)
# F-values (almost) hold, p-values (especially for skill) are off
(mixed3 <- mixed(induct ~ cond + skill + (1|room:cond), md_16.4))

# however, parameters are perfectly recovered when using the original contrasts:
mixed3_orig <- mixed(induct ~ cond + skill + (1|room:cond), md_16.4b, check_contrasts=FALSE)
summary(mixed3_orig)


### replicate results from Table 16.10 (Maxwell & Delaney, 2004, p. 862)
# for this we need to center cog:
md_16.4b$cog <- scale(md_16.4b$cog, scale=FALSE)

# F-values and p-values are relatively off:
(mixed4 <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b))
# contrast has a relatively important influence on cog
(mixed4_orig <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b, check_contrasts=FALSE))

# parameters are again almost perfectly recovered:
summary(mixed4_orig)

####################
## Other Examples ##
####################

\dontrun{

# use the obk.long data (not reasonable, no random slopes)
data(obk.long)
mixed(value ~ treatment * phase + (1|id), obk.long)

# Examples for using the per.parammeter argument:
data(obk.long, package = "afex")
obk.long$hour <- ordered(obk.long$hour)

# tests only the main effect parameters of hour individually per parameter.
mixed(value ~ treatment*phase*hour +(1|id), per_parameter = "^hour$", data = obk.long)

# tests all parameters including hour individually
mixed(value ~ treatment*phase*hour +(1|id), per_parameter = "hour", data = obk.long)

# tests all parameters individually
mixed(value ~ treatment*phase*hour +(1|id), per_parameter = ".", data = obk.long)

# example data from package languageR:
# Lexical decision latencies elicited from 21 subjects for 79 English concrete nouns, 
# with variables linked to subject or word. 
data(lexdec, package = "languageR")

# using the simplest model
m1 <- mixed(RT ~ Correct + Trial + PrevType * meanWeight + 
    Frequency + NativeLanguage * Length + (1|Subject) + (1|Word), data = lexdec)
m1
#                  Effect         df         F p.value
# 1               Correct 1, 1627.73   8.15 **    .004
# 2                 Trial 1, 1592.43   7.57 **    .006
# 3              PrevType 1, 1605.39      0.17     .68
# 4            meanWeight   1, 75.39 14.85 ***   .0002
# 5             Frequency   1, 76.08 56.53 ***  <.0001
# 6        NativeLanguage   1, 27.11      0.70     .41
# 7                Length   1, 75.83   8.70 **    .004
# 8   PrevType:meanWeight 1, 1601.18    6.18 *     .01
# 9 NativeLanguage:Length 1, 1555.49 14.24 ***   .0002

# Fitting a GLMM using parametric bootstrap:
require("mlmRev") # for the data, see ?Contraception

gm1 <- mixed(use ~ age + I(age^2) + urban + livch + (1 | district), method = "PB",
 family = binomial, data = Contraception, args_test = list(nsim = 10))

#######################
### Using Multicore ###
#######################

require(parallel)
(nc <- detectCores()) # number of cores
cl <- makeCluster(rep("localhost", nc)) # make cluster
# to keep track of what the function is doindg redirect output to outfile:
# cl <- makeCluster(rep("localhost", nc), outfile = "cl.log.txt")

## There are two ways to use multicore:

# 1. Obtain fits with multicore:
mixed(value ~ treatment*phase*hour +(1|id), data = obk.long, method = "LRT", cl = cl)

# 2. Obtain PB samples via multicore: 
mixed(use ~ age + I(age^2) + urban + livch + (1 | district), family = binomial,
 method = "PB", data = Contraception, args_test = list(nsim = 10, cl = cl))

## Both ways can be combined:
mixed(use ~ age + I(age^2) + urban + livch + (1 | district), family = binomial, 
 method = "PB", data = Contraception, args_test = list(nsim = 10, cl = cl), cl = cl)


#### use all_fit = TRUE and expand_re = TRUE:
data("sk2011.2") 
sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])

require(optimx) # uses two more algorithms
sk2_aff_b <- mixed(response ~ instruction*type+(inference*type||id), sk2_aff,
               expand_re = TRUE, all_fit = TRUE)
attr(sk2_aff_b, "all_fit_selected")
attr(sk2_aff_b, "all_fit_logLik")

# considerably faster with multicore:
clusterEvalQ(cl, library(optimx)) # need to load optimx in cluster
sk2_aff_b2 <- mixed(response ~ instruction*type+(inference*type||id), sk2_aff,
               expand_re = TRUE, all_fit = TRUE, cl=cl)
attr(sk2_aff_b2, "all_fit_selected")
attr(sk2_aff_b2, "all_fit_logLik")


stopCluster(cl)

}
