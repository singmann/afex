## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------
options(width = 90)
knitr::opts_chunk$set(dpi=100)

## ----message=FALSE, warning=FALSE-------------------------------------------------------
library("afex")     # needed for ANOVA functions.
library("emmeans")  # emmeans must now be loaded explicitly for follow-up tests.
library("multcomp") # for advanced control for multiple testing/Type 1 errors.
library("ggplot2")  # for customizing plots.
afex_options(emmeans_model = "multivariate") # use multivariate model for all follow-up tests.

## ---------------------------------------------------------------------------------------
data(sk2011.1)
str(sk2011.1)


## ---------------------------------------------------------------------------------------
with(sk2011.1, table(inference, id, plausibility))

## ---------------------------------------------------------------------------------------
a1 <- aov_ez("id", "response", sk2011.1, between = "instruction", 
       within = c("inference", "plausibility"))
a1 # the default print method prints a data.frame produced by nice 

## ---- eval=FALSE------------------------------------------------------------------------
#  aov_car(response ~ instruction + Error(id/inference*plausibility), sk2011.1)
#  aov_4(response ~ instruction + (inference*plausibility|id), sk2011.1)

## ---- results='asis'--------------------------------------------------------------------
knitr::kable(nice(a1))

## ---- results='asis'--------------------------------------------------------------------
print(xtable::xtable(anova(a1), digits = c(rep(2, 5), 3, 4)), type = "html")

## ---------------------------------------------------------------------------------------
m1 <- emmeans(a1, ~ inference)
m1

## ---------------------------------------------------------------------------------------
pairs(m1)

## ---------------------------------------------------------------------------------------
summary(as.glht(pairs(m1)), test=adjusted("free"))

## ---------------------------------------------------------------------------------------
m2 <- emmeans(a1, "inference", by = "instruction")
## equal: emmeans(a1, ~ inference|instruction)
m2

## ---------------------------------------------------------------------------------------
pairs(m2)

## ---------------------------------------------------------------------------------------
m3 <- emmeans(a1, c("inference", "instruction"))
## equal: emmeans(a1, ~inference*instruction)
m3
pairs(m3)


## ---------------------------------------------------------------------------------------
c1 <- list(
  v_i.ded = c(0.5, 0.5, -0.5, -0.5, 0, 0, 0, 0),
  v_i.prob = c(0, 0, 0, 0, 0.5, 0.5, -0.5, -0.5)
  )

contrast(m3, c1, adjust = "holm")
summary(as.glht(contrast(m3, c1)), test = adjusted("free"))

## ----fig.width=7.5, fig.height=4--------------------------------------------------------
afex_plot(a1, x = "inference", trace = "instruction", panel = "plausibility")

## ----fig.width=7.5, fig.height=4--------------------------------------------------------
afex_plot(a1, x = "inference", trace = "instruction", panel = "plausibility", 
          error = "within")

## ----fig.width=7.5, fig.height=4--------------------------------------------------------
afex_plot(a1, x = "inference", trace = "instruction", panel = "plausibility", 
          error = "none")

## ----fig.width=7.5, fig.height=4--------------------------------------------------------
p1 <- afex_plot(a1, x = "inference", trace = "instruction", 
                panel = "plausibility", error = "none", 
                mapping = c("color", "fill"), 
                data_geom = geom_boxplot, data_arg = list(width = 0.4), 
                point_arg = list(size = 1.5), line_arg = list(size = 1))
p1

## ----fig.width=7.5, fig.height=4--------------------------------------------------------
p1 + theme_light()

## ---------------------------------------------------------------------------------------
theme_set(theme_light())

## ---------------------------------------------------------------------------------------
a2 <- aov_ez("id", "response", sk2011.1, between = "instruction", 
       within = c("validity", "plausibility", "what"))
a2

## ----fig.width=7.5, fig.height=4--------------------------------------------------------
afex_plot(a2, x = c("plausibility", "validity"), 
          trace = "instruction", panel = "what", 
          error = "none")

## ---------------------------------------------------------------------------------------
(m4 <- emmeans(a2, ~instruction+plausibility+validity|what))
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
summary(as.glht(contrast(m4, c2)), test = adjusted("free"))

