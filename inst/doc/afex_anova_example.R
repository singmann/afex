## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------
options(width = 90)

## ----message=FALSE, warning=FALSE-------------------------------------------------------
require(afex) # needed for ANOVA, lsmeans is loaded automatically.
require(multcomp) # for advanced control for multiple testing/Type 1 errors.
require(lattice) # for plots

## ---------------------------------------------------------------------------------------
data(sk2011.1)
str(sk2011.1)


## ---------------------------------------------------------------------------------------
with(sk2011.1, table(inference, id, plausibility))

## ---------------------------------------------------------------------------------------
a1 <- aov_ez("id", "response", sk2011.1, between = "instruction", 
       within = c("inference", "plausibility"))
a1 # the default print method prints a data.frame produced by nice 

## ---- results='asis'--------------------------------------------------------------------
knitr::kable(nice(a1))

## ---- results='asis'--------------------------------------------------------------------
print(xtable::xtable(anova(a1), digits = c(rep(2, 5), 3, 4)), type = "html")

## ---------------------------------------------------------------------------------------
m1 <- lsmeans(a1, ~ inference)
m1

## ---------------------------------------------------------------------------------------
pairs(m1)

## ---------------------------------------------------------------------------------------
summary(as.glht(pairs(m1)), test=adjusted("free"))

## ---------------------------------------------------------------------------------------
m2 <- lsmeans(a1, ~ inference|instruction)
m2

## ---------------------------------------------------------------------------------------
pairs(m2)

## ---------------------------------------------------------------------------------------
m3 <- lsmeans(a1, ~ inference:instruction)
m3
pairs(m3)


## ---------------------------------------------------------------------------------------
c1 <- list(
  v_i.ded = c(0.5, 0.5, -0.5, -0.5, 0, 0, 0, 0),
  v_i.prob = c(0, 0, 0, 0, 0.5, 0.5, -0.5, -0.5)
  )

contrast(m3, c1, adjust = "holm")
summary(as.glht(contrast(m3, c1)), test =adjusted("free"))

## ----fig.width=7.5, fig.height=4--------------------------------------------------------
lsmip(a1, instruction ~ inference|plausibility)

## ---------------------------------------------------------------------------------------
a2 <- aov_ez("id", "response", sk2011.1, between = "instruction", 
       within = c("validity", "plausibility", "what"))
a2

## ----fig.width=7.5, fig.height=4--------------------------------------------------------
lsmip(a2, ~instruction ~ plausibility+validity|what, 
      scales = list(x=list(
        at = 1:4,
        labels = c("pl:v", "im:v", "pl:i", "im:i")
        )))

## ---------------------------------------------------------------------------------------
(m4 <- lsmeans(a2, ~instruction+plausibility+validity|what))
c2 <- list(
  diff_1 = c(1, -1, 0, 0, 0, 0, 0, 0),
  diff_2 = c(0, 0, 1, -1, 0, 0, 0, 0),
  diff_3 = c(0, 0, 0, 0,  1, -1, 0, 0),
  diff_4 = c(0, 0, 0, 0,  0, 0, 1, -1),
  val_ded  = c(0.5, 0, 0.5, 0, -0.5, 0, -0.5, 0),
  val_prob = c(0, 0.5, 0, 0.5, 0, -0.5, 0, -0.5),
  plau_ded   = c(0.5, 0, -0.5, 0, -0.5, 0, 0.5, 0),
  plau_prob  = c(0, 0.5, 0, -0.5, 0, 0.5, 0, -0.5)
  )
contrast(m4, c2, adjust = "holm")

## ---------------------------------------------------------------------------------------
summary(as.glht(contrast(m4, c2[1:4])), test =adjusted("free"))
summary(as.glht(contrast(m4, c2[5:6])), test =adjusted("free"))
summary(as.glht(contrast(m4, c2[7:8])), test =adjusted("free"))

