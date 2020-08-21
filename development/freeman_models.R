
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

