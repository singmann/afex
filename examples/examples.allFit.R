
\dontrun{

# basic usage
require(optimx)
gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             data = cbpp, family = binomial)
gm_all <- allFit(gm1)
t(sapply(gm_all,fixef)) ## extract fixed effects
sapply(gm_all,logLik) ## log-likelihoods
sapply(gm_all,getME,"theta") ## theta parameters
!sapply(gm_all,inherits,"try-error") ## was fit OK?


## use allFit in combination with expand.re = TRUE
data("sk2011.2") # see example("mixed")
sk_m2 <- mixed(response ~ instruction*inference*type+(inference*type||id), sk2_aff,
               expand_re = TRUE)
sk_m2
sk_m2_allFit <- allFit(sk_m2$full.model)
sk_m2_allFit # all fits fail

sk2_aff <- mixed(response ~ instruction*inference*type+(inference*type||id), sk2_aff,
               expand_re = TRUE, return = "data") # returns data only
sk_m2_allFit <- allFit(sk_m2$full.model) # works now
t(sapply(sk_m2_allFit,fixef))
sapply(sk_m2_allFit,logLik)

}
