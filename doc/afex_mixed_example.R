## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------
options(width = 90)
knitr::opts_chunk$set(dpi=72)

## ----message=FALSE, warning=FALSE-------------------------------------------------------
library("afex") # needed for mixed() and attaches lme4 automatically.
library("emmeans") # emmeans is needed for follow-up tests (and not anymore loaded automatically).
library("multcomp") # for advanced control for multiple testing/Type 1 errors.
library("dplyr") # for working with data frames
library("tidyr") # for transforming data frames from wide to long and the other way round.
library("lattice") # for plots
library("latticeExtra") # for combining lattice plots, etc.
lattice.options(default.theme = standard.theme(color = FALSE)) # black and white
lattice.options(default.args = list(as.table = TRUE)) # better ordering

data("fhch2010") # load 
fhch <- droplevels(fhch2010[ fhch2010$correct,]) # remove errors
str(fhch2010) # structure of the data

## ---------------------------------------------------------------------------------------
## are all participants in only one task?
fhch2010 %>% group_by(id) %>%
  summarise(task = n_distinct(task)) %>%
  as.data.frame() %>% 
  {.$task == 1} %>%
  all()

## participants per condition:
fhch2010 %>% group_by(id) %>%
  summarise(task = first(task)) %>%
  ungroup() %>%
  group_by(task) %>%
  summarise(n = n())

## number of different items per participant:
fhch2010 %>% group_by(id, stimulus) %>%
  summarise(items = n_distinct(item)) %>%
  ungroup() %>%
  group_by(stimulus) %>%
  summarise(min = min(items), 
            max = max(items), 
            mean = mean(items))



## ---- fig.width=7, fig.height=4---------------------------------------------------------
fhch_long <- fhch %>% gather("rt_type", "rt", rt, log_rt)
histogram(~rt|rt_type, fhch_long, breaks = "Scott", type = "density",
          scale = list(x = list(relation = "free")))

## ---- fig.width=7, fig.height=6---------------------------------------------------------
agg_p <- fhch %>% group_by(id, task, stimulus, density, frequency) %>%
  summarise(mean = mean(log_rt)) %>%
  ungroup()

xyplot(mean ~ density:frequency|task+stimulus, agg_p, jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex =1.5)
       }) + 
bwplot(mean ~ density:frequency|task+stimulus, agg_p, pch="|", do.out = FALSE)

## ---- fig.width=7, fig.height=6---------------------------------------------------------
agg_i <- fhch %>% group_by(item, task, stimulus, density, frequency) %>%
  summarise(mean = mean(log_rt)) %>%
  ungroup()

xyplot(mean ~ density:frequency|task+stimulus, agg_i, jitter.x = TRUE, pch = 20, alpha = 0.2, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex =1.5)
       }) + 
bwplot(mean ~ density:frequency|task+stimulus, agg_i, pch="|", do.out = FALSE)

## ---- eval = FALSE----------------------------------------------------------------------
#  
#  m1s <- mixed(log_rt ~ task*stimulus*density*frequency + (stimulus*density*frequency|id)+
#                 (task|item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)))
#  m2s <- mixed(log_rt ~ task*stimulus*density*frequency + (stimulus*density*frequency|id)+
#                 (task||item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)
#  m3s <- mixed(log_rt ~ task*stimulus*density*frequency + (stimulus*density*frequency||id)+
#                 (task|item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)
#  m4s <- mixed(log_rt ~ task*stimulus*density*frequency + (stimulus*density*frequency||id)+
#                 (task||item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

## ---------------------------------------------------------------------------------------
load(system.file("extdata/", "freeman_models.rda", package = "afex"))
m1s
m2s
m3s
m4s

## ---- eval = FALSE----------------------------------------------------------------------
#  m1lrt <- mixed(log_rt ~ task*stimulus*density*frequency + (stimulus*density*frequency|id)+
#                   (task|item), fhch, method = "LRT",
#                 control = lmerControl(optCtrl = list(maxfun = 1e6)))
#  m2lrt <- mixed(log_rt ~ task*stimulus*density*frequency + (stimulus*density*frequency|id)+
#                   (task||item), fhch, method = "LRT",
#                 control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)
#  m3lrt <- mixed(log_rt ~ task*stimulus*density*frequency + (stimulus*density*frequency||id)+
#                   (task|item), fhch, method = "LRT",
#                 control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)
#  m4lrt <- mixed(log_rt ~ task*stimulus*density*frequency + (stimulus*density*frequency||id)+
#                   (task||item), fhch, method = "LRT",
#                 control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

## ---------------------------------------------------------------------------------------
res_lrt <- cbind(nice_lrt[[1]], "  " = " ", 
                 nice_lrt[[4]][,-(1:2)])
colnames(res_lrt)[c(3,4,6,7)] <- paste0(
  rep(c("m1_", "m4_"), each =2), colnames(res_lrt)[c(3,4)])
res_lrt

## ---------------------------------------------------------------------------------------
nice_lrt[[2]]

## ---------------------------------------------------------------------------------------
emm_options(lmer.df = "asymptotic") # also possible: 'satterthwaite', 'kenward-roger'
emm_i1 <- emmeans(m2s, "frequency", by = c("stimulus", "task"))
emm_i1

## ---------------------------------------------------------------------------------------
update(pairs(emm_i1), by = NULL, adjust = "holm")

## ---- eval=FALSE------------------------------------------------------------------------
#  summary(as.glht(update(pairs(emm_i1), by = NULL)), test = adjusted("free"))

## ---------------------------------------------------------------------------------------
emm_i1b <- summary(contrast(emm_i1, by = NULL))
emm_i1b[,c("estimate", "SE")] <- exp(emm_i1b[,c("estimate", "SE")])
emm_i1b

## ---------------------------------------------------------------------------------------
emm_i2 <- emmeans(m2s, c("density", "frequency"), by = c("stimulus", "task"))
con1 <- contrast(emm_i2, "trt.vs.ctrl1", by = c("frequency", "stimulus", "task")) # density
con2 <- contrast(con1, "trt.vs.ctrl1", by = c("contrast", "stimulus", "task")) 
test(con2, joint = TRUE, by = c("stimulus", "task"))

## ---------------------------------------------------------------------------------------
emm_i2
# desired contrats:
des_c <- list(
  ll_hl = c(1, -1, 0, 0),
  lh_hh = c(0, 0, 1, -1)
  )
update(contrast(emm_i2, des_c), by = NULL, adjust = "holm")

## ---- echo=FALSE, eval = FALSE----------------------------------------------------------
#  ### OLD STUFF BELOW. PLEASE IGNORE.
#  load("freeman_models.rda")
#  load("../freeman_models_all.rda")
#  m1lrt$restricted_models <- list(NULL)
#  m2lrt$restricted_models <- list(NULL)
#  m3lrt$restricted_models <- list(NULL)
#  m4lrt$restricted_models <- list(NULL)
#  
#  save(m1lrt, file = "freeman_models1.rda", compress = "xz")
#  save(m1s, m2s, m3s, m4s, m1lrt, m2lrt, m3lrt, m4lrt, file = "freeman_models.rda", compress = "xz")
#  
#  anovas_lrt <- lapply(list(m1lrt, m2lrt, m3lrt, m4lrt), anova)
#  nice_lrt <- lapply(list(m1lrt, m2lrt, m3lrt, m4lrt), nice)
#  
#  res_lrt <- cbind(nice_lrt[[1]], "  " = " ",
#                   nice_lrt[[2]][,-(1:2)], "  " = " ",
#                   nice_lrt[[3]][,-(1:2)], "  " = " ",
#                   nice_lrt[[4]][,-(1:2)])
#  colnames(res_lrt)[c(3,4,6,7, 9,10, 12,13)] <- paste0(
#    rep(c("m1_", "m2_", "m3_","m4_"), each =2), colnames(res_lrt)[c(3,4)])
#  
#  ## warnings:
#  m1s # fails and 1 warning
#  m2s # 1 warning
#  m3s # 0 warnings
#  m4s # 0 warnings
#  
#  m1lrt # 11 warnings
#  m2lrt # 1 nested model(s) provide better, 7 other warnings
#  m3lrt # 7 nested models provide better fit, 9 other warnings
#  m4lrt # 0 warnings
#  
#  cbind(nice_lrt[[1]]$Effect, do.call("cbind", lapply(nice_lrt, function(x) x[,3:4])))
#  
#  save(m1s, m2s, m3s, m4s, anovas_lrt, nice_lrt,file = "freeman_models.rda", compress = "xz")
#  save(m1s, m2s, m3s, m4s, m1lrt, m2lrt, m3lrt, m4lrt, file = "freeman_models2.rda", compress = "bzip2")
#  tools::resaveRdaFiles("freeman_models1.rda")
#  

