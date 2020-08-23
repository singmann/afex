
capture_call <- function(call) {
  warnings <- testthat::capture_warnings(eval(substitute(call)))
  output <- suppressWarnings(capture.output(eval(substitute(call))))
  messages <- testthat::capture_messages(substitute(call))
  list(
    output = output,
    warnings = warnings,
    messages = messages
  )
}
load("development/freeman_models.rda")

### outputs
outp_m4s_vc <- capture_call(summary(m4s)$varcor )
outp_m5s_vc <- capture_call(summary(m5s)$varcor )
outp_m6s_vc <- capture_call(summary(m6s)$varcor )

fit_m8s <- capture_call(
  m8s <- mixed(log_rt ~ task*stimulus*density*frequency + 
                 (stimulus+frequency|id)+
                 (task||item), fhch, method = "S", 
               control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)
)
fit_m8s$warnings

outp_comb_anova <- capture_call(left_join(nice(m1s), nice(m9s), by = "Effect", 
          suffix = c("_full", "_final")))

outp_m9lrt <- capture_call(m9lrt)

###
emm_options(lmer.df = "asymptotic") # also possible: 'satterthwaite', 'kenward-roger'
emm_i1 <- emmeans(m9s, "frequency", by = c("stimulus", "task"))
emm_i1b <- update(emm_i1, tran = "log", type = "response", by = NULL)

emm_o1 <- capture_call(emm_i1)

emm_o2 <- capture_call(update(pairs(emm_i1), by = NULL, adjust = "holm"))

emm_o3 <- capture_call(summary(as.glht(update(pairs(emm_i1), by = NULL)), 
                               test = adjusted("free")))
emm_o4 <- capture_call(emm_i1b)

p1 <- afex_plot(m9s, "frequency", "stimulus", "task", id = "id",
          data_geom = ggbeeswarm::geom_quasirandom, 
          data_arg = list(
            dodge.width = 0.5,  ## needs to be same as dodge
            cex = 0.8,
            color = "darkgrey"))

emm_o5 <- capture_call(joint_tests(m9s, by = c("stimulus", "task")))


emm_i2 <- emmeans(m2s, c("density", "frequency"), by = c("stimulus", "task"))
emm_o6 <- capture_call(emm_i2)
# desired contrats:
des_c <- list(
  ll_hl = c(1, -1, 0, 0),
  lh_hh = c(0, 0, 1, -1)
  )

emm_o7 <- capture_call(update(contrast(emm_i2, des_c), by = NULL, adjust = "holm"))

### save outputs

save(outp_m4s_vc, outp_m5s_vc, outp_m6s_vc, 
     fit_m8s, outp_comb_anova, 
     outp_m9lrt,
     emm_o1, emm_o2, emm_o3, emm_o4, emm_o5, emm_o6, emm_o7,
     p1,
     file = "inst/extdata/output_mixed_vignette.rda", compress = "xz")
#save(m9s, file = "inst/extdata/freeman_final.rda", compress = "xz")

### afex_plot vignette
library("cowplot")
emmeans::emm_options(lmer.df = "asymptotic")
aout_1 <- capture_call(m9s)
aout_2 <- capture_call(
  pairs(emmeans::emmeans(m9s, c("stimulus", "frequency"), by = "task"))
)

ap1 <- afex_plot(m9s, x = "stimulus", trace = "frequency", panel = "task") 
ap2 <- plot_grid( 
  afex_plot(m9s, x = "stimulus", trace = "frequency", panel = "task", 
            id = "id"), 
  afex_plot(m9s, x = "stimulus", trace = "frequency", panel = "task", 
            id = "item"), 
  labels = c("ID", "Item") 
)
ap3 <- plot_grid( 
  afex_plot(m9s, x = "stimulus", trace = "frequency", panel = "task", 
            id = "item", dodge = 0.8,
            data_geom = geom_violin, 
            data_arg = list(width = 0.5)), 
  afex_plot(m9s, x = "stimulus", trace = "frequency", panel = "task", 
            id = "item", dodge = 0.8,
            data_geom = geom_boxplot, 
            data_arg = list(width = 0.5),
            error_arg = list(size = 1.5, width = 0, linetype = 1))
)
ap4 <- plot_grid( 
  afex_plot(m9s, x = "stimulus", trace = "frequency", panel = "task", 
            id = "id", error = "within"),
  afex_plot(m9s, x = "stimulus", trace = "frequency", panel = "task", 
            id = "item", dodge = 0.8, error = "within",
            data_geom = geom_violin, 
            data_arg = list(width = 0.5))
)

save(aout_1, aout_2,
     ap1, ap2, ap3, ap4,
     file = "inst/extdata/output_afex_plot_mixed_vignette.rda", 
     compress = "xz")


message("boundary (singular) fit: see ?isSingular")
#### fit models ####

m1s <- mixed(log_rt ~ task*stimulus*density*frequency + 
               (stimulus*density*frequency|id)+
               (task|item), fhch, method = "S", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)))
m2s <- mixed(log_rt ~ task*stimulus*density*frequency + 
               (stimulus*density*frequency|id)+
               (task||item), fhch, method = "S", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)
m3s <- mixed(log_rt ~ task*stimulus*density*frequency + 
               (stimulus*density*frequency||id)+
               (task|item), fhch, method = "S", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)
m4s <- mixed(log_rt ~ task*stimulus*density*frequency + 
               (stimulus*density*frequency||id)+
               (task||item), fhch, method = "S", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

summary(m4s)


m5s <- mixed(log_rt ~ task*stimulus*density*frequency + 
               ((stimulus+density+frequency)^2||id)+
               (task||item), fhch, method = "S", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

summary(m5s)

m6s <- mixed(log_rt ~ task*stimulus*density*frequency + 
               (stimulus+density+frequency||id)+
               (task||item), fhch, method = "S", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

summary(m6s)

m7s <- mixed(log_rt ~ task*stimulus*density*frequency + 
               (stimulus+frequency||id)+
               (task||item), fhch, method = "S", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

summary(m7s)

m8s <- mixed(log_rt ~ task*stimulus*density*frequency + 
               (stimulus+frequency|id)+
               (task||item), fhch, method = "S", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

summary(m8s)

m9s <- mixed(log_rt ~ task*stimulus*density*frequency + 
               (stimulus+frequency||id)+
               (task|item), fhch, method = "S", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

summary(m9s)

m9lrt <- mixed(log_rt ~ task*stimulus*density*frequency + 
               (stimulus+frequency||id)+
               (task|item), fhch, method = "LRT", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)), 
             expand_re = TRUE)
summary(m9lrt)

m7lrt <- mixed(log_rt ~ task*stimulus*density*frequency + 
               (stimulus+frequency||id)+
               (task||item), fhch, method = "LRT", 
             control = lmerControl(optCtrl = list(maxfun = 1e6)), expand_re = TRUE)

summary(m7lrt)

save(m1s, m2s, m3s, m4s, m5s, m6s, m7s, m8s, m9s, 
     m7lrt, m9lrt, file = "development/freeman_models.rda", 
     compress = "xz")

