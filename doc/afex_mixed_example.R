## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------
op <- options(width = 90, dplyr.summarise.inform = FALSE)
knitr::opts_chunk$set(dpi=72)

## ---- echo=FALSE------------------------------------------------------------------------
load(system.file("extdata/", "output_mixed_vignette.rda", package = "afex"))

## ----message=FALSE, warning=FALSE-------------------------------------------------------
library("afex") # needed for mixed() and attaches lme4 automatically.
library("emmeans") # emmeans is needed for follow-up tests 
library("multcomp") # for advanced control for multiple testing/Type 1 errors.
library("dplyr") # for working with data frames
library("tidyr") # for transforming data frames from wide to long and the other way round.
library("ggplot2") # for plots
theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom", 
                  panel.grid.major.x = element_blank()))

data("fhch2010") # load 
fhch <- droplevels(fhch2010[ fhch2010$correct,]) # remove errors
str(fhch2010) # structure of the data

## ---------------------------------------------------------------------------------------
## are all participants in only one task?
fhch %>% group_by(id) %>%
  summarise(task = n_distinct(task)) %>%
  as.data.frame() %>% 
  {.$task == 1} %>%
  all()

## participants per condition:
fhch %>% group_by(id) %>%
  summarise(task = first(task)) %>%
  ungroup() %>%
  group_by(task) %>%
  summarise(n = n())

## number of different items per participant:
fhch %>% group_by(id, stimulus) %>%
  summarise(items = n_distinct(item)) %>%
  ungroup() %>%
  group_by(stimulus) %>%
  summarise(min = min(items), 
            max = max(items), 
            mean = mean(items))



## ---- fig.width=7, fig.height=4---------------------------------------------------------
fhch_long <- fhch %>% 
  pivot_longer(cols = c(rt, log_rt), names_to = "rt_type", values_to = "rt")
ggplot(fhch_long, aes(rt)) +
  geom_histogram(bins = 100) +
  facet_wrap(vars(rt_type), scales = "free_x")

## ---- fig.width=7, fig.height=6, message=FALSE, out.width="90%"-------------------------
agg_p <- fhch %>% 
  group_by(id, task, stimulus, density, frequency) %>%
  summarise(mean = mean(log_rt)) %>%
  ungroup()

ggplot(agg_p, aes(x = interaction(density,frequency), y = mean)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.5) +
  geom_boxplot(fill = "transparent") +
  stat_summary(colour = "red") +
  facet_grid(cols = vars(task), rows = vars(stimulus))

## ---- fig.width=7, fig.height=6, message=FALSE, out.width="90%"-------------------------
agg_i <- fhch %>% group_by(item, task, stimulus, density, frequency) %>%
  summarise(mean = mean(log_rt)) %>%
  ungroup()

ggplot(agg_i, aes(x = interaction(density,frequency), y = mean)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.3) +
  geom_boxplot(fill = "transparent") +
  stat_summary(colour = "red") +
  facet_grid(cols = vars(task), rows = vars(stimulus))

## ---- eval = FALSE----------------------------------------------------------------------
#  m1s <- mixed(log_rt ~ task*stimulus*density*frequency +
#                 (stimulus*density*frequency|id)+
#                 (task|item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)))

## ---- echo=FALSE------------------------------------------------------------------------
message("boundary (singular) fit: see ?isSingular")

## ---- eval = FALSE----------------------------------------------------------------------
#  m2s <- mixed(log_rt ~ task*stimulus*density*frequency +
#                 (stimulus*density*frequency|id)+
#                 (task||item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

## ---- echo=FALSE------------------------------------------------------------------------
message("boundary (singular) fit: see ?isSingular")

## ---- eval = FALSE----------------------------------------------------------------------
#  m3s <- mixed(log_rt ~ task*stimulus*density*frequency +
#                 (stimulus*density*frequency||id)+
#                 (task|item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

## ---- echo=FALSE------------------------------------------------------------------------
message("boundary (singular) fit: see ?isSingular")

## ---- eval = FALSE----------------------------------------------------------------------
#  m4s <- mixed(log_rt ~ task*stimulus*density*frequency +
#                 (stimulus*density*frequency||id)+
#                 (task||item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

## ---- echo=FALSE------------------------------------------------------------------------
message("boundary (singular) fit: see ?isSingular")

## ---- eval=FALSE------------------------------------------------------------------------
#  summary(m4s)$varcor

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_m4s_vc$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  m5s <- mixed(log_rt ~ task*stimulus*density*frequency +
#                 ((stimulus+density+frequency)^2||id)+
#                 (task||item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

## ---- echo=FALSE------------------------------------------------------------------------
message("boundary (singular) fit: see ?isSingular")

## ---- eval=FALSE------------------------------------------------------------------------
#  summary(m5s)$varcor

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_m5s_vc$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  m6s <- mixed(log_rt ~ task*stimulus*density*frequency +
#                 (stimulus+density+frequency||id)+
#                 (task||item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)),
#               expand_re = TRUE)

## ---- echo=FALSE------------------------------------------------------------------------
message("boundary (singular) fit: see ?isSingular")

## ---- eval=FALSE------------------------------------------------------------------------
#  summary(m6s)$varcor

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_m6s_vc$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  m7s <- mixed(log_rt ~ task*stimulus*density*frequency +
#                 (stimulus+frequency||id)+
#                 (task||item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  m8s <- mixed(log_rt ~ task*stimulus*density*frequency +
#                 (stimulus+frequency|id)+
#                 (task||item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

## ---- echo=FALSE------------------------------------------------------------------------
warning(fit_m8s$warnings, call. = FALSE)

## ---- eval=FALSE------------------------------------------------------------------------
#  m9s <- mixed(log_rt ~ task*stimulus*density*frequency +
#                 (stimulus+frequency||id)+
#                 (task|item), fhch, method = "S",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  left_join(nice(m1s), nice(m9s), by = "Effect",
#            suffix = c("_full", "_final"))

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_comb_anova$output, sep = "\n")

## ---- eval = FALSE----------------------------------------------------------------------
#  m9lrt <- mixed(log_rt ~ task*stimulus*density*frequency +
#                 (stimulus+frequency||id)+
#                 (task|item), fhch, method = "LRT",
#               control = lmerControl(optCtrl = list(maxfun = 1e6)),
#               expand_re = TRUE)
#  m9lrt

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_m9lrt$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emm_options(lmer.df = "asymptotic") # also possible: 'satterthwaite', 'kenward-roger'
#  emm_i1 <- emmeans(m9s, "frequency", by = c("stimulus", "task"))
#  emm_i1

## ---- echo=FALSE------------------------------------------------------------------------
message("NOTE: Results may be misleading due to involvement in interactions")
cat(emm_o1$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  update(pairs(emm_i1), by = NULL, adjust = "holm")

## ---- echo=FALSE------------------------------------------------------------------------
message("NOTE: Results may be misleading due to involvement in interactions")
cat(emm_o2$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  summary(as.glht(update(pairs(emm_i1), by = NULL)), test = adjusted("free"))

## ---- echo=FALSE------------------------------------------------------------------------
cat(emm_o3$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emm_i1b <- update(emm_i1, tran = "log", type = "response", by = NULL)
#  emm_i1b

## ---- echo=FALSE------------------------------------------------------------------------
cat(emm_o4$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  afex_plot(m9s, "frequency", "stimulus", "task", id = "id",
#            data_geom = ggbeeswarm::geom_quasirandom,
#            data_arg = list(
#              dodge.width = 0.5,  ## needs to be same as dodge
#              cex = 0.8,
#              color = "darkgrey"))

## ---- echo=FALSE, fig.width=5, fig.height=4, out.width="90%"----------------------------
p1

## ---- eval=FALSE------------------------------------------------------------------------
#  joint_tests(m9s, by = c("stimulus", "task"))

## ---- echo=FALSE------------------------------------------------------------------------
cat(emm_o5$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emm_i2 <- emmeans(m2s, c("density", "frequency"), by = c("stimulus", "task"))
#  emm_i2

## ---- echo=FALSE------------------------------------------------------------------------
cat(emm_o6$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  # desired contrats:
#  des_c <- list(
#    ll_hl = c(1, -1, 0, 0),
#    lh_hh = c(0, 0, 1, -1)
#    )
#  update(contrast(emm_i2, des_c), by = NULL, adjust = "holm")

## ---- echo=FALSE------------------------------------------------------------------------
cat(emm_o7$output, sep = "\n")

## ---- include=FALSE-----------------------------------------------------------
options(op)

