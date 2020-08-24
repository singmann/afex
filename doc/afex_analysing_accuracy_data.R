## ---- include = FALSE-------------------------------------------------------------------
op <- options(width = 90, dplyr.summarise.inform = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE
)

## ----setup, message=FALSE, results='hide', warning=FALSE--------------------------------
library("afex")
library("emmeans")
library("dplyr")
library("ggplot2")
theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom", 
                  panel.grid.major.x = element_blank()))
library("cowplot")
library("ggbeeswarm")

## ---------------------------------------------------------------------------------------
data("stroop")
## extract data from experiment 1 and remove NAs
stroop_e1 <- stroop %>%
  filter(!is.na(acc)) %>% 
  filter(study == "1") %>% 
  droplevels()

## ---------------------------------------------------------------------------------------
head(stroop_e1)
str(stroop_e1)

## ---------------------------------------------------------------------------------------
e1_anova <- aov_ez(
  id = "pno", 
  dv = "acc", 
  data = stroop_e1,
  within = c("congruency", "condition")
)


## ---------------------------------------------------------------------------------------
e1_anova

## ---------------------------------------------------------------------------------------
emmeans(e1_anova, "congruency", model = "multivariate")

## ---------------------------------------------------------------------------------------
emmeans(e1_anova, "condition", model = "multivariate")

## ---- fig.width=6, fig.height=3---------------------------------------------------------
plot_grid(
  afex_plot(e1_anova, "congruency", error = "within", 
            data_geom = geom_quasirandom, data_alpha = 0.3) + 
    coord_cartesian(ylim = c(0.25, 1)),
  afex_plot(e1_anova, "condition", error = "within", 
            data_geom = geom_quasirandom, data_alpha = 0.3) +
    coord_cartesian(ylim = c(0.25, 1))
)

## ---- echo=FALSE------------------------------------------------------------------------
load(system.file("extdata/", "outputs_glmm_vignette.rda", package = "afex"))

## ---- eval=FALSE, warning=FALSE---------------------------------------------------------
#  e1_mixed1_v1 <- mixed(
#    acc ~ congruency*condition + (congruency*condition|pno),
#    data = stroop_e1,
#    method = "LRT",
#    family = binomial
#  )

## ---------------------------------------------------------------------------------------
stroop_e1_agg <- stroop_e1 %>% 
  group_by(condition, congruency, pno) %>% 
  summarise(acc = mean(acc), 
            n = n())

## ---- eval = FALSE----------------------------------------------------------------------
#  e1_mixed1_v2 <- mixed(
#    acc ~ congruency*condition + (congruency*condition|pno),
#    data = stroop_e1_agg,
#    method = "LRT",
#    family = binomial,
#    weight = n
#  )

## ---- echo=FALSE------------------------------------------------------------------------
xxx <- lapply(outp_e1_mixed1_v1$warnings, 
              function(x) warning(x, call. = FALSE))

## ---- echo=FALSE------------------------------------------------------------------------
xxx <- lapply(outp_e1_mixed1_v2$warnings, 
              function(x) warning(x, call. = FALSE))

## ---- eval = FALSE----------------------------------------------------------------------
#  e1_mixed1_v2_allfit <- mixed(
#    acc ~ congruency*condition + (congruency*condition|pno),
#    data = stroop_e1_agg,
#    method = "LRT",
#    family = binomial,
#    weight = n,
#    all_fit = TRUE
#  )
#  

## ---- echo=FALSE------------------------------------------------------------------------
xxx <- lapply(outp_e1_mixed1_v2_allfit$warnings, 
              function(x) warning(x, call. = FALSE))

## ---- eval=FALSE------------------------------------------------------------------------
#  e1_mixed1_v1 ## variant 1

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_e1_mixed1_v1$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  e1_mixed1_v2  ## variant 2

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_e1_mixed1_v2$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  e1_mixed1_v2_allfit  ## variant 2 with all_fit = TRUE

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_e1_mixed1_v2_allfit$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emmeans(e1_mixed1_v2_allfit, "congruency", type = "response")

## ---- echo=FALSE------------------------------------------------------------------------
message("NOTE: Results may be misleading due to involvement in interactions")
cat(emm_2a_cong_out$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emmeans(e1_mixed1_v2_allfit, "condition", type = "response")

## ---- echo=FALSE------------------------------------------------------------------------
message("NOTE: Results may be misleading due to involvement in interactions")
cat(emm_2a_cond_out$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  plot_grid(
#    afex_plot(e1_mixed1_v2_allfit, "congruency",
#              data_geom = geom_quasirandom, data_alpha = 0.3) +
#      coord_cartesian(ylim = c(0.25, 1)),
#    afex_plot(e1_mixed1_v2_allfit, "condition",
#              data_geom = geom_quasirandom, data_alpha = 0.3) +
#      coord_cartesian(ylim = c(0.25, 1))
#  )

## ---- echo=FALSE, fig.width=6, fig.height=3---------------------------------------------
message("Aggregating data over: pno")
message("NOTE: Results may be misleading due to involvement in interactions")
message("Aggregating data over: pno")
message("NOTE: Results may be misleading due to involvement in interactions")
plot_grid(pp2a_main_a, pp2a_main_b)

## ---- eval=FALSE------------------------------------------------------------------------
#  emmeans(e1_mixed1_v2_allfit, c("congruency", "condition"), type = "response")

## ---- echo=FALSE------------------------------------------------------------------------
cat(emm_2a_inter1_out$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emm_inter_1 <- emmeans(e1_mixed1_v2_allfit, "congruency",
#                         by = "condition", type = "response")
#  emm_inter_1

## ---- echo=FALSE------------------------------------------------------------------------
cat(emm_2a_inter2_out$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  pairs(emm_inter_1)

## ---- echo=FALSE------------------------------------------------------------------------
cat(emm_2a_pairs$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  afex_plot(e1_mixed1_v2_allfit, "condition", "congruency",
#            data_geom = geom_violin)

## ---- echo=FALSE, fig.width=4.5, fig.height=3.5-----------------------------------------
message("Aggregating data over: pno")
pp2a_inter

## ---- eval=FALSE------------------------------------------------------------------------
#  afex_plot(e1_mixed1_v1, "condition", "congruency",
#            data_geom = geom_violin)

## ---- echo=FALSE, fig.width=4.5, fig.height=3.5-----------------------------------------
message("Aggregating data over: pno")
pp2a_inter_v1

## ---- eval=FALSE, include=FALSE---------------------------------------------------------
#  save(e1_mixed1_v1, e1_mixed1_v2, e1_mixed1_v2_allfit,
#       e12_mixed1_t2, e12_mixed1, e12_mixed1_t2_red,
#       file = "development/stroop_mixed.rda", compress = "xz")
#  ## load("development/stroop_mixed.rda")

## ---- eval=FALSE, include=FALSE---------------------------------------------------------
#  stroop_e1_agg <- stroop_e1 %>%
#    group_by(condition, congruency, pno) %>%
#    summarise(acc = mean(acc),
#              n = n())
#  capture_call <- function(call) {
#    warnings <- testthat::capture_warnings(eval(substitute(call)))
#    output <- suppressWarnings(capture.output(eval(substitute(call))))
#    messages <- testthat::capture_messages(substitute(call))
#    list(
#      output = output,
#      warnings = warnings,
#      messages = messages
#    )
#  }
#  outp_e1_mixed1_v1 <- capture_call(print(e1_mixed1_v1))
#  outp_e1_mixed1_v2 <- capture_call(print(e1_mixed1_v2))
#  outp_e1_mixed1_v2_allfit <- capture_call(print(e1_mixed1_v2_allfit))
#  
#  emm_2a_cong <- emmeans(e1_mixed1_v2_allfit, "congruency", type = "response")
#  emm_2a_cond <- emmeans(e1_mixed1_v2_allfit, "condition", type = "response")
#  
#  emm_2a_inter1 <- emmeans(e1_mixed1_v2_allfit, c("congruency", "condition"),
#                           type = "response")
#  emm_2a_inter2 <- emmeans(e1_mixed1_v2_allfit, "congruency",
#                           by = "condition", type = "response")
#  
#  emm_2a_inter1_out <- capture_call(print(emm_2a_inter1))
#  emm_2a_inter2_out <- capture_call(print(emm_2a_inter2))
#  
#  
#  emm_2a_cong_out <- capture_call(print(emm_2a_cong))
#  emm_2a_cond_out <- capture_call(print(emm_2a_cond))
#  
#  emm_inter_1 <- emmeans(e1_mixed1_v2_allfit, "congruency",
#                         by = "condition", type = "response")
#  emm_2a_pairs <- capture_call(print(pairs(emm_inter_1)))
#  
#  # pp2a_main <- plot_grid(
#  #   afex_plot(e1_mixed1_v2_allfit, "congruency", error = "within",
#  #             data_geom = geom_quasirandom, data_alpha = 0.3) +
#  #     coord_cartesian(ylim = c(0.25, 1)),
#  #   afex_plot(e1_mixed1_v2_allfit, "condition", error = "within",
#  #             data_geom = geom_quasirandom, data_alpha = 0.3) +
#  #     coord_cartesian(ylim = c(0.25, 1))
#  # )
#  pp2a_main_a <- afex_plot(e1_mixed1_v2_allfit, "congruency", error = "within",
#              data_geom = geom_quasirandom, data_alpha = 0.3) +
#      coord_cartesian(ylim = c(0.25, 1))
#  pp2a_main_b <- afex_plot(e1_mixed1_v2_allfit, "condition", error = "within",
#              data_geom = geom_quasirandom, data_alpha = 0.3) +
#      coord_cartesian(ylim = c(0.25, 1))
#  
#  pp2a_inter <- afex_plot(e1_mixed1_v2_allfit, "condition", "congruency",
#            data_geom = geom_violin)
#  
#  pp2a_inter_v1 <- afex_plot(e1_mixed1_v1, "condition", "congruency",
#            data_geom = geom_violin)
#  
#  ### experiments 1 and 2
#  
#  outp_e12_mixed1 <- capture_call(print(e12_mixed1))
#  outp_e12_mixed1_t2 <- capture_call(print(e12_mixed1_t2))
#  
#  outp_e12_mixed1_t2_red <- capture_call(print(e12_mixed1_t2_red))
#  
#  emm_e12_inter_out <- capture_call(print(emmeans(e12_mixed1,
#                                      c("congruency", "condition"),
#                                      type = "response")))
#  
#  emm_e12_t2_inter_out <- capture_call(print(emmeans(e12_mixed1_t2,
#                                      c("congruency", "condition"),
#                                      type = "response")))
#  
#  emm_e12_t2_red_inter_out <- capture_call(print(emmeans(e12_mixed1_t2_red,
#                             c("congruency", "condition"),
#                             type = "response")))
#  
#  emm_e12_t2_red_pairs_out <- capture_call(print(pairs(
#    emmeans(e12_mixed1_t2_red, "congruency",
#                                      by = "condition",
#                                      type = "response"))))
#  
#  emm_e12_t2_pairs_out <- capture_call(print(pairs(
#    emmeans(e12_mixed1_t2, "congruency",
#                                  by = "condition", type = "response"))))
#  
#  emm_e12_t2_pairs_p_out <- capture_call(print(pairs(emmeans(e12_mixed1_t2, "congruency", by = "condition", type = "response",
#          submodel = "minimal"))))
#  
#  pp_e12_inter_t3 <- afex_plot(e12_mixed1, "condition", "congruency",
#                               data_geom = geom_violin)
#  pp_e12_inter_t2 <- afex_plot(e12_mixed1_t2, "condition", "congruency",
#                               data_geom = geom_violin)
#  pp_e12_inter_t2_red <- afex_plot(e12_mixed1_t2_red, "condition", "congruency",
#                                   data_geom = geom_violin)
#  
#  save(outp_e1_mixed1_v1, outp_e1_mixed1_v2,
#       outp_e1_mixed1_v2_allfit,
#       emm_2a_cong_out, emm_2a_cond_out,
#       pp2a_main_a, pp2a_main_b,
#       emm_2a_inter1_out, emm_2a_inter2_out,
#       emm_2a_pairs,
#       pp2a_inter, pp2a_inter_v1,
#  
#       outp_e12_mixed1, outp_e12_mixed1_t2,
#       outp_e12_mixed1_t2_red,
#       emm_e12_inter_out, emm_e12_t2_inter_out,
#       emm_e12_t2_red_inter_out,
#       emm_e12_t2_red_pairs_out,
#       emm_e12_t2_pairs_out,
#       emm_e12_t2_pairs_p_out,
#       pp_e12_inter_t3, pp_e12_inter_t2,
#       pp_e12_inter_t2_red,
#  
#       file = "inst/extdata/outputs_glmm_vignette.rda",
#       compress = "xz")
#  

## ---------------------------------------------------------------------------------------
## extract data from experiment 1 and remove NAs
stroop_e12 <- stroop %>%
  filter(!is.na(acc)) %>% 
  filter(study %in% c("1", "2")) %>% 
  droplevels()

## ---------------------------------------------------------------------------------------
stroop_e12 %>% 
  group_by(study) %>% 
  summarise(n = length(unique(pno)))

## ---------------------------------------------------------------------------------------
stroop_e12_agg <- stroop_e12 %>% 
  group_by(study, condition, congruency, pno) %>% 
  summarise(acc = mean(acc), 
            n = n())

## ---- eval = FALSE----------------------------------------------------------------------
#  library("parallel")
#  nc <- detectCores() # number of cores
#  cl <- makeCluster(rep("localhost", nc)) # make cluster

## ---- eval = FALSE----------------------------------------------------------------------
#  e12_mixed1 <- mixed(
#    acc ~ congruency*condition*study + (congruency*condition|pno),
#    data = stroop_e12_agg,
#    method = "LRT",
#    family = binomial,
#    weight = n,
#    all_fit = TRUE,
#    cl = cl
#  )

## ---- eval = FALSE----------------------------------------------------------------------
#  e12_mixed1_t2 <- mixed(
#    acc ~ congruency*condition*study + (congruency*condition|pno),
#    data = stroop_e12_agg,
#    method = "LRT",
#    family = binomial,
#    weight = n,
#    all_fit = TRUE,
#    cl = cl,
#    type = 2
#  )

## ---- eval=FALSE------------------------------------------------------------------------
#  e12_mixed1

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_e12_mixed1$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  e12_mixed1_t2

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_e12_mixed1_t2$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  afex_plot(e12_mixed1, "condition", "congruency",
#            data_geom = geom_violin)

## ---- echo=FALSE, fig.width=4.5, fig.height=3.5-----------------------------------------
message("Aggregating data over: pno")
message("NOTE: Results may be misleading due to involvement in interactions")
pp_e12_inter_t3

## ---- eval=FALSE------------------------------------------------------------------------
#  afex_plot(e12_mixed1_t2, "condition", "congruency",
#            data_geom = geom_violin)

## ---- echo=FALSE, fig.width=4.5, fig.height=3.5-----------------------------------------
message("Aggregating data over: pno")
message("emmeans are based on full model which includes all effects.")
message("NOTE: Results may be misleading due to involvement in interactions")
pp_e12_inter_t2

## ---- eval=FALSE------------------------------------------------------------------------
#  emmeans(e12_mixed1, c("congruency", "condition"), type = "response")

## ---- echo=FALSE------------------------------------------------------------------------
message("NOTE: Results may be misleading due to involvement in interactions")
cat(emm_e12_inter_out$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emmeans(e12_mixed1_t2, c("congruency", "condition"), type = "response")

## ---- echo=FALSE------------------------------------------------------------------------
message("NOTE: Results may be misleading due to involvement in interactions")
cat(emm_e12_t2_inter_out$output, sep = "\n")

## ---- eval = FALSE----------------------------------------------------------------------
#  e12_mixed1_t2_red <- mixed(
#    acc ~ (congruency+condition+study)^2 + (congruency*condition|pno),
#    data = stroop_e12_agg,
#    method = "LRT",
#    family = binomial,
#    weight = n,
#    all_fit = TRUE,
#    cl = cl,
#    type = 2
#  )

## ---- eval=FALSE------------------------------------------------------------------------
#  e12_mixed1_t2_red

## ---- echo=FALSE------------------------------------------------------------------------
cat(outp_e12_mixed1_t2_red$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emmeans(e12_mixed1_t2_red,  c("congruency", "condition"), type = "response")

## ---- echo=FALSE------------------------------------------------------------------------
message("emmeans are based on full model which includes all effects.")
cat(emm_e12_t2_red_inter_out$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emmeans(e12_mixed1_t2_red, "congruency", by = "condition",
#          type = "response") %>%
#    pairs()

## ---- echo=FALSE------------------------------------------------------------------------
message("emmeans are based on full model which includes all effects.")
cat(emm_e12_t2_red_pairs_out$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emmeans(e12_mixed1_t2, "congruency", by = "condition", type = "response") %>%
#    pairs()

## ---- echo=FALSE------------------------------------------------------------------------
message("emmeans are based on full model which includes all effects.")
message("NOTE: Results may be misleading due to involvement in interactions")
cat(emm_e12_t2_pairs_out$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  emmeans(e12_mixed1_t2, "congruency", by = "condition", type = "response",
#          submodel = "minimal") %>%
#    pairs()

## ---- echo=FALSE------------------------------------------------------------------------
message("emmeans are based on full model which includes all effects.")
message("NOTE: Results may be misleading due to involvement in interactions")
cat(emm_e12_t2_pairs_p_out$output, sep = "\n")

## ---- eval=FALSE------------------------------------------------------------------------
#  afex_plot(e12_mixed1_t2_red, "condition", "congruency",
#            data_geom = geom_violin)

## ---- echo=FALSE, fig.width=4.5, fig.height=3.5-----------------------------------------
message("Aggregating data over: pno")
message("emmeans are based on full model which includes all effects.")
pp_e12_inter_t2_red

## ---- include=FALSE-----------------------------------------------------------
options(op)

