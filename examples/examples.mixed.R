### replicate results from Table 15.4 (Maxwell & Delaney, 2004, p. 789)
data(md_15.1)
# random intercept plus slope
(t15.4a <- mixed(iq ~ timecat + (1+time|id),data=md_15.1))

# to also replicate exact parameters use treatment.contrasts and the last level as base level:
contrasts(md_15.1$timecat) <- contr.treatment(4, base = 4)
(t15.4b <- mixed(iq ~ timecat + (1+time|id),data=md_15.1, check.contrasts=FALSE))
summary(t15.4a)  # gives "wrong" parameters extimates
summary(t15.4b)  # identical parameters estimates

# for more examples from chapter 15 see ?md_15.1

### replicate results from Table 16.3 (Maxwell & Delaney, 2004, p. 837)
data(md_16.1)

# original results need treatment contrasts:
(mixed1_orig <- mixed(severity ~ sex + (1|id), md_16.1, check.contrasts=FALSE))
summary(mixed1_orig$full.model)

# p-value stays the same with afex default contrasts (contr.sum),
# but estimates and t-values for the fixed effects parameters change.
(mixed1 <- mixed(severity ~ sex + (1|id), md_16.1))
summary(mixed1$full.model)


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
(mixed2_orig <- mixed(induct ~ cond + (1|room:cond), md_16.4b, check.contrasts=FALSE))
summary(mixed2_orig$full.model) # replicates parameters


### replicate results from Table 16.7 (Maxwell & Delaney, 2004, p. 851)
# F-values (almost) hold, p-values (especially for skill) are off
(mixed3 <- mixed(induct ~ cond + skill + (1|room:cond), md_16.4))

# however, parameters are perfectly recovered when using the original contrasts:
mixed3_orig <- mixed(induct ~ cond + skill + (1|room:cond), md_16.4b, check.contrasts=FALSE)
summary(mixed3_orig)



### replicate results from Table 16.10 (Maxwell & Delaney, 2004, p. 862)
# for this we need to center cog:
md_16.4b$cog <- scale(md_16.4b$cog, scale=FALSE)

# F-values and p-values are relatively off:
(mixed4 <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b))
# contrast has a relatively important influence on cog
(mixed4_orig <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b, check.contrasts=FALSE))

# parameters are again almost perfectly recovered:
summary(mixed4_orig)



\dontrun{

# use the obk.long data (not reasonable, no random slopes)
data(obk.long)
mixed(value ~ treatment * phase + (1|id), obk.long)

# Examples for using the per.parammeter argument:
data(obk.long, package = "afex")
obk.long$hour <- ordered(obk.long$hour)

# tests only the main effect parameters of hour individually per parameter.
mixed(value ~ treatment*phase*hour +(1|id), per.parameter = "^hour$", data = obk.long)

# tests all parameters including hour individually
mixed(value ~ treatment*phase*hour +(1|id), per.parameter = "hour", data = obk.long)

# tests all parameters individually
mixed(value ~ treatment*phase*hour +(1|id), per.parameter = ".", data = obk.long)

# example data from package languageR:
# Lexical decision latencies elicited from 21 subjects for 79 English concrete nouns, 
# with variables linked to subject or word. 
data(lexdec, package = "languageR")

# using the simplest model
m1 <- mixed(RT ~ Correct + Trial + PrevType * meanWeight + 
    Frequency + NativeLanguage * Length + (1|Subject) + (1|Word), data = lexdec)

m1
##                  Effect  stat ndf     ddf F.scaling p.value
## 1               Correct  8.15   1 1627.73      1.00    .004
## 2                 Trial  7.57   1 1592.43      1.00    .006
## 3              PrevType  0.17   1 1605.39      1.00     .68
## 4            meanWeight 14.85   1   75.39      1.00   .0002
## 5             Frequency 56.53   1   76.08      1.00  <.0001
## 6        NativeLanguage  0.70   1   27.11      1.00     .41
## 7                Length  8.70   1   75.83      1.00    .004
## 8   PrevType:meanWeight  6.18   1 1601.18      1.00     .01
## 9 NativeLanguage:Length 14.24   1 1555.49      1.00   .0002

# Fitting a GLMM using parametric bootstrap:
require("mlmRev") # for the data, see ?Contraception

gm1 <- mixed(use ~ age + I(age^2) + urban + livch + (1 | district), method = "PB",
 family = binomial, data = Contraception, args.test = list(nsim = 10))

#######################
### using multicore ###
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
 method = "PB", data = Contraception, args.test = list(nsim = 10, cl = cl))

## Both ways can be combined:
mixed(use ~ age + I(age^2) + urban + livch + (1 | district), family = binomial, 
 method = "PB", data = Contraception, args.test = list(nsim = 10, cl = cl), cl = cl)

stopCluster(cl)

}
