## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------
options(width = 90)
knitr::opts_chunk$set(dpi=72)
knitr::knit_hooks$set(document = function(x){
  gsub("```\n*```r*\n*", "", x)
})

## ----message=FALSE, warning=FALSE-------------------------------------------------------
library("afex")     
library("ggplot2")  
library("cowplot")
theme_set(theme_bw(base_size = 14) + 
            theme(legend.position="bottom", 
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank()))

## ---------------------------------------------------------------------------------------
set_sum_contrasts()

## ---------------------------------------------------------------------------------------
warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)

## ----fig.width=7, fig.height=3----------------------------------------------------------
p1 <- afex_plot(warp.lm, "tension")
p2 <- afex_plot(warp.lm, "tension", "wool")
plot_grid(p1, p2)

## ---------------------------------------------------------------------------------------
ins <- data.frame(
    n = c(500, 1200, 100, 400, 500, 300),
    size = factor(rep(1:3,2), labels = c("S","M","L")),
    age = factor(rep(1:2, each = 3)),
    claims = c(42, 37, 1, 101, 73, 14))

## ----fig.width=3, fig.height=3----------------------------------------------------------
ins.glm <- glm(claims ~ size + age + offset(log(n)), 
               data = ins, family = "poisson")
afex_plot(ins.glm, "size", "age")

## ---------------------------------------------------------------------------------------
## binomial glm adapted from ?predict.glm
ldose <- factor(rep(0:5, 2))
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- numdead/20  ## dv should be a vector, no matrix
budworm.lg <- glm(SF ~ sex*ldose, family = binomial, 
                  weights = rep(20, length(numdead)))

## ----fig.width=8, fig.height=3----------------------------------------------------------
a <- afex_plot(budworm.lg, "ldose")
b <- afex_plot(budworm.lg, "ldose", "sex") ## data point is hidden behind mean!
c <- afex_plot(budworm.lg, "ldose", "sex", 
          data_arg = list(size = 4, color = "red"))
plot_grid(a, b, c, labels = "AUTO", nrow = 1)

## ----fig.width=8, fig.height=6----------------------------------------------------------
## nlme mixed model
data(Oats, package = "nlme")
Oats$nitro <- factor(Oats$nitro)
oats.1 <- nlme::lme(yield ~ nitro * Variety, 
                    random = ~ 1 | Block / Variety,
                    data = Oats)
plot_grid(
  afex_plot(oats.1, "nitro", "Variety", data = Oats), # A
  afex_plot(oats.1, "nitro", "Variety", data = Oats), # B
  afex_plot(oats.1, "nitro", "Variety", data = Oats, id = "Block"), # C
  afex_plot(oats.1, "nitro", data = Oats), # D
  afex_plot(oats.1, "nitro", data = Oats, id = c("Block", "Variety")), # E
  afex_plot(oats.1, "nitro", data = Oats, id = "Block"), # F
  labels = "AUTO"
)

## ---- eval=FALSE------------------------------------------------------------------------
#  library("glmmTMB")
#  tmb <- glmmTMB(count~spp * mined + (1|site),
#                ziformula = ~spp * mined,
#                family=nbinom2, Salamanders)
#  

## ---- eval=FALSE, include=FALSE---------------------------------------------------------
#  library("glmmTMB")
#  set_sum_contrasts()
#  tmb <- glmmTMB(count~spp * mined + (1|site),
#                ziformula = ~spp * mined,
#                family=nbinom2, Salamanders)
#  save(tmb, file = "inst/extdata/tmb_example_fit.rda", compress = "xz")

## ---- echo=FALSE, include=FALSE---------------------------------------------------------
library("glmmTMB")
data(Salamanders, package = "glmmTMB")
load(system.file("extdata/", "tmb_example_fit.rda", package = "afex"))

## ----fig.width=8, fig.height=3----------------------------------------------------------
plot_grid(
  afex_plot(tmb, "spp"),
  afex_plot(tmb, "spp", data_geom = geom_violin),
  afex_plot(tmb, "spp", id = "site", data = Salamanders), 
  labels = "AUTO", nrow = 1
)

## ----fig.width=8.5, fig.height=3.5------------------------------------------------------
a <- afex_plot(tmb, "spp", "mined")
b <- afex_plot(tmb, "spp", "mined", data_alpha = 0.3,
          data_arg = list(
            position = 
              ggplot2::position_jitterdodge(
                jitter.width = 0.2, 
                jitter.height = 0.5, 
                dodge.width = 0.5  ## needs to be same as dodge
                ),
            color = "darkgrey"))
plot_grid(a, b, labels = "AUTO")

## ----fig.width=5.5, fig.height=3.5------------------------------------------------------
afex_plot(tmb, "spp", "mined", id = "site", data = Salamanders, 
          data_geom = ggbeeswarm::geom_beeswarm, 
          data_arg = list(dodge.width = 0.5, cex = 0.4,
                          color = "darkgrey")
          )

## ---- eval=FALSE------------------------------------------------------------------------
#  library("rstanarm") ## requires resetting the ggplot2 theme
#  theme_set(theme_bw(base_size = 14) +
#              theme(legend.position="bottom",
#                    panel.grid.major.x = element_blank(),
#                    panel.grid.minor.x = element_blank()))
#  cbpp <- lme4::cbpp
#  cbpp$prob <- with(cbpp, incidence / size)
#  example_model <- stan_glmer(prob ~ period + (1|herd),
#                              data = cbpp, family = binomial, weight = size,
#                              chains = 2, cores = 1, seed = 12345, iter = 500)

## ---- eval=FALSE------------------------------------------------------------------------
#  b1 <- afex_plot(example_model, "period")
#  ## dv column detected: prob
#  ## No id column passed. Assuming all rows are independent samples.
#  b2 <- afex_plot(example_model, "period", data_geom = geom_violin)
#  ## dv column detected: prob
#  ## No id column passed. Assuming all rows are independent samples.
#  plot_grid(b1, b2, labels = "AUTO")

## ----fig.width=7, fig.height=3, echo=FALSE----------------------------------------------
load(system.file("extdata/", "plots_rstanarm.rda", package = "afex"))
plot_grid(b1, b2, labels = "AUTO")

## ---- eval=FALSE------------------------------------------------------------------------
#  cbpp_l <- vector("list", nrow(cbpp))
#  for (i in seq_along(cbpp_l)) {
#    cbpp_l[[i]] <- data.frame(
#      herd = cbpp$herd[i],
#      period = cbpp$period[i],
#      incidence = rep(0, cbpp$size[i])
#    )
#    cbpp_l[[i]]$incidence[seq_len(cbpp$incidence[i])] <- 1
#  }
#  cbpp_l <- do.call("rbind", cbpp_l)
#  cbpp_l$herd <- factor(cbpp_l$herd, levels = levels(cbpp$herd))
#  cbpp_l$period <- factor(cbpp_l$period, levels = levels(cbpp$period))
#  example_model2 <- stan_glmer(incidence ~ period + (1|herd),
#                               data = cbpp_l, family = binomial,
#                               chains = 2, cores = 1, seed = 12345, iter = 500)

## ---- eval=FALSE------------------------------------------------------------------------
#  b3 <- afex_plot(example_model2, "period")
#  ## dv column detected: incidence
#  ## No id column passed. Assuming all rows are independent samples.
#  b4 <- afex_plot(example_model2, "period", id = "herd")
#  ## dv column detected: incidence
#  plot_grid(b3, b4, labels = "AUTO")

## ----fig.width=7, fig.height=3, echo=FALSE----------------------------------------------
plot_grid(b3, b4, labels = "AUTO")

## ---- eval=FALSE------------------------------------------------------------------------
#  data("Machines", package = "MEMSS")
#  mm <- stan_lmer(score ~ Machine + (Machine|Worker), data=Machines,
#                  chains = 2, cores = 1, seed = 12345, iter = 500)

## ---- eval=FALSE------------------------------------------------------------------------
#  b5 <- afex_plot(mm, "Machine")
#  ## dv column detected: score
#  ## No id column passed. Assuming all rows are independent samples.
#  b6 <- afex_plot(mm, "Machine", id = "Worker")
#  ## dv column detected: score
#  plot_grid(b5, b6, labels = "AUTO")

## ----fig.width=7, fig.height=3, echo=FALSE----------------------------------------------
plot_grid(b5, b6, labels = "AUTO")

## ---- eval=FALSE, include=FALSE---------------------------------------------------------
#  library("rstanarm") ## requires resetting the ggplot2 theme
#  library("ggplot2")
#  theme_set(theme_bw(base_size = 14) +
#              theme(legend.position="bottom",
#                    panel.grid.major.x = element_blank(),
#                    panel.grid.minor.x = element_blank()))
#  set_sum_contrasts()
#  cbpp <- lme4::cbpp
#  cbpp$prob <- with(cbpp, incidence / size)
#  example_model <- stan_glmer(prob ~ period + (1|herd),
#                              data = cbpp, family = binomial, weight = size,
#                              chains = 2, cores = 1, seed = 12345, iter = 500)
#  b1 <- afex_plot(example_model, "period")
#  b2 <- afex_plot(example_model, "period", data_geom = geom_violin)
#  
#  cbpp_l <- vector("list", nrow(cbpp))
#  for (i in seq_along(cbpp_l)) {
#    cbpp_l[[i]] <- data.frame(
#      herd = cbpp$herd[i],
#      period = cbpp$period[i],
#      incidence = rep(0, cbpp$size[i])
#    )
#    cbpp_l[[i]]$incidence[seq_len(cbpp$incidence[i])] <- 1
#  }
#  cbpp_l <- do.call("rbind", cbpp_l)
#  cbpp_l$herd <- factor(cbpp_l$herd, levels = levels(cbpp$herd))
#  cbpp_l$period <- factor(cbpp_l$period, levels = levels(cbpp$period))
#  example_model2 <- stan_glmer(incidence ~ period + (1|herd),
#                               data = cbpp_l, family = binomial,
#                               chains = 2, cores = 1, seed = 12345, iter = 500)
#  b3 <- afex_plot(example_model2, "period")
#  b4 <- afex_plot(example_model2, "period", id = "herd")
#  
#  data("Machines", package = "MEMSS")
#  mm <- stan_lmer(score ~ Machine + (Machine|Worker), data=Machines,
#                  chains = 2, cores = 1, seed = 12345, iter = 500)
#  b5 <- afex_plot(mm, "Machine")
#  b6 <- afex_plot(mm, "Machine", id = "Worker", data = Machines)
#  save(b1, b2, b3, b4, b5, b6, file = "../inst/extdata/plots_rstanarm.rda",
#       compress = "xz", version = 2)

## ---- eval=FALSE------------------------------------------------------------------------
#  library("brms")
#  data("Machines", package = "MEMSS")
#  mm2 <- brm(score ~ Machine + (Machine|Worker), data=Machines,
#             chains = 2, cores = 1, seed = 12345, iter = 500)

## ---- eval=FALSE------------------------------------------------------------------------
#  bb1 <- afex_plot(mrt, "Machine", data = Machines, dv = "score")
#  ## No id column passed. Assuming all rows are independent samples.
#  bb2 <- afex_plot(mm, "Machine", id = "Worker",
#            data = Machines, dv = "score")
#  plot_grid(bb1, bb2)

## ----fig.width=7, fig.height=3, echo=FALSE----------------------------------------------
load(system.file("extdata/", "plots_brms.rda", package = "afex"))
plot_grid(bb1, bb2)

## ---- eval=FALSE, include=FALSE---------------------------------------------------------
#  library("brms")
#  data("Machines", package = "MEMSS")
#  mm2 <- brm(score ~ Machine + (Machine|Worker), data=Machines,
#             chains = 2, cores = 1, seed = 12345, iter = 500)
#  bb1 <- afex_plot(mm2, "Machine", data = Machines, dv = "score")
#  bb2 <- afex_plot(mm2, "Machine", id = "Worker",
#            data = Machines, dv = "score")
#  save(bb1, bb2, file = "../inst/extdata/plots_brms.rda", version = 2)

## ----fig.width=4, fig.height=3, eval = FALSE--------------------------------------------
#  library("GLMMadaptive")
#  data(Salamanders, package = "glmmTMB")
#  gm1 <- mixed_model(count~spp * mined, random = ~ 1 | site, data = Salamanders,
#                     family = zi.poisson(), zi_fixed = ~ mined)
#  
#  afex_plot(gm1, "spp", data = Salamanders)

