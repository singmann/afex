## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------
options(width = 90)
knitr::opts_chunk$set(dpi=72)

## ----message=FALSE, warning=FALSE-------------------------------------------------------
library("afex")     
library("ggplot2")  
library("cowplot")
theme_set(theme_grey())

## ---------------------------------------------------------------------------------------
data(md_12.1)
(aw <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise")))

## ----fig.width=9, fig.height=4----------------------------------------------------------
p_an <- afex_plot(aw, x = "angle", trace = "noise") 
p_na <- afex_plot(aw, x = "noise", trace = "angle")
plot_grid(p_an, p_na)  ## try adding: labels = "AUTO"

## ---------------------------------------------------------------------------------------
p_an <- afex_plot(aw, x = "angle", trace = "noise", error = "within",
                  factor_levels = list(angle = c("0°", "4°", "8°"),
                                    noise = c("Absent", "Present")), 
                  legend_title = "Noise") +
  labs(y = "RTs (in ms)", x = "Angle (in degrees)")

## ----fig.width=8.5, fig.height=6, dpi = 150---------------------------------------------
plot_grid(
  p_an + theme_bw() + theme(legend.position="bottom"),
  p_an + theme_light() + theme(legend.position="bottom"),
  p_an + theme_minimal() + theme(legend.position="bottom"),
  p_an + jtools::theme_nice() + theme(legend.position="bottom"),
  p_an + ggpubr::theme_pubr(),
  p_an + theme_cowplot() + theme(legend.position="bottom"),
  labels = "AUTO"
)  

## ----fig.width=3.5, fig.height=3, dpi = 100, out.width='50%'----------------------------
p_an + 
  scale_y_continuous(breaks=seq(400, 900, length.out = 3)) +
  theme_bw(base_size = 15) + 
  theme(legend.position="bottom", 
        panel.grid.major.x = element_blank())

## ---------------------------------------------------------------------------------------
theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom", 
                  panel.grid.major.x = element_blank()))

## ---- eval=FALSE------------------------------------------------------------------------
#  ggsave("my_plot.png", device = "png",
#         width = 9, height = 8, units = "cm",
#         dpi = 600) ## the larger the dpi, the better the resolution

## ---- eval=FALSE------------------------------------------------------------------------
#  ggsave("my_plot.pdf", device = "pdf",
#         width = 9, height = 8, units = "cm")

## ----fig.width=8.5, fig.height=12, dpi = 125--------------------------------------------
p1 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.3,
                data_arg = list(
                  position = 
                    ggplot2::position_jitterdodge(
                      jitter.width = 0, 
                      jitter.height = 10, 
                      dodge.width = 0.3  ## needs to be same as dodge
                    ),
                  color = "darkgrey"))
p2 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.5,
                data_geom = ggbeeswarm::geom_beeswarm,
                data_arg = list(
                  dodge.width = 0.5,  ## needs to be same as dodge
                  cex = 0.8,
                  color = "darkgrey"))
p3 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.5,
                data_geom = geom_count)
p4 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", 
                data_geom = ggplot2::geom_violin, 
                data_arg = list(width = 0.5))
p5 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", 
                data_geom = ggplot2::geom_boxplot, 
                data_arg = list(width = 0.3, linetype = 1))
p6 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.7, 
                data_geom = ggpol::geom_boxjitter, 
                data_arg = list(
                  width = 0.5, 
                  jitter.width = 0,
                  jitter.height = 10,
                  outlier.intersect = TRUE),
                point_arg = list(size = 2.5), 
                error_arg = list(size = 1.5, width = 0))
plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, labels = 1:6)  

## ----fig.width=8.5, fig.height=8, dpi = 125---------------------------------------------
p2 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.5,
                mapping = c("shape", "color"),
                data_geom = ggbeeswarm::geom_beeswarm,
                data_arg = list(
                  dodge.width = 0.5,  ## needs to be same as dodge
                  cex = 0.8))
p3 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", 
                mapping = c("linetype", "shape", "fill"),
                data_geom = ggplot2::geom_violin, 
                data_arg = list(width = 0.5))
p4 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", 
                mapping = c("shape", "fill"),
                data_geom = ggplot2::geom_boxplot, 
                data_arg = list(width = 0.3))
p5 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.7,
                mapping = c("shape", "fill"),
                data_geom = ggpol::geom_boxjitter, 
                data_arg = list(
                  width = 0.5, 
                  jitter.width = 0,
                  jitter.height = 10,
                  outlier.intersect = TRUE),
                point_arg = list(size = 2.5), 
                line_arg = list(linetype = 0),
                error_arg = list(size = 1.5, width = 0))
plot_grid(p2, p3, p4, p5, ncol = 2, labels = 2:5) 

## ----fig.width=8.5, fig.height=4, dpi = 150---------------------------------------------
p1 <- afex_plot(aw, x = "noise", trace = "angle", mapping = "color", 
                error = "within", 
                point_arg = list(size = 5), line_arg = list(size = 2),
                error_arg = list(size = 2))
p2 <- afex_plot(aw, x = "noise", trace = "angle", 
                mapping = c("color", "shape", "linetype"), 
                error = "within", 
                point_arg = list(size = 5), line_arg = list(size = 2),
                error_arg = list(size = 2, width = 0, linetype = 1))
plot_grid(p1, p2, ncol = 2)

## ----fig.width=7, fig.height=3.5, message=FALSE-----------------------------------------
po1 <- afex_plot(aw, x = "angle", mapping = "color", error = "within", 
                 data_arg = list(),
                 point_arg = list(size = 2.5), 
                 error_arg = list(size = 1.5, width = 0.05)) 
po2 <- afex_plot(aw, x = "angle", error = "within", 
                 data_geom = ggpol::geom_boxjitter, 
                 mapping = "fill", data_alpha = 0.7, 
                 data_arg = list(
                   width = 0.6, 
                   jitter.width = 0.05,
                   jitter.height = 10,
                   outlier.intersect = TRUE
                 ),
                 point_arg = list(size = 2.5), 
                 error_arg = list(size = 1.5, width = 0.05)) +
  theme(legend.position="none")
plot_grid(po1, po2) 

## ----fig.width=7, fig.height=3.5, message=FALSE-----------------------------------------
afex_plot(aw, x = "angle", panel = "noise", error = "within",
          data_geom = ggpol::geom_boxjitter,
          mapping = "fill", data_alpha = 0.7,
          data_arg = list(
            width = 0.6,
            jitter.width = 0.05,
            jitter.height = 10,
            outlier.intersect = TRUE
          ),
          point_arg = list(size = 2.5),
          error_arg = list(size = 1.5, width = 0.05)) +
  theme(legend.position="none")

## ----fig.width=7, fig.height=3.5, message=FALSE-----------------------------------------
plot_grid(
  po1 + geom_line(aes(group = 1), color = "darkgrey", size = 1.5), 
  po2 + geom_line(aes(group = 1))
) 

## ---- eval=FALSE, echo=FALSE, results='hide'--------------------------------------------
#  data("fhch2010") # load
#  fhch <- droplevels(fhch2010[ fhch2010$correct,]) # remove errors
#  ### following model should take less than a minute to fit:
#  mrt <- mixed(log_rt ~ task*stimulus*frequency + (stimulus*frequency||id)+
#                 (task||item), fhch, method = "S", expand_re = TRUE)
#  save(mrt, file = "../inst/extdata/freeman_reduced_model.rda", compress = "xz")

## ---- eval=FALSE------------------------------------------------------------------------
#  data("fhch2010") # load
#  fhch <- droplevels(fhch2010[ fhch2010$correct,]) # remove errors
#  ### following model should take less than a minute to fit:
#  mrt <- mixed(log_rt ~ task*stimulus*frequency + (stimulus*frequency||id)+
#                 (task||item), fhch, method = "S", expand_re = TRUE)

## ---------------------------------------------------------------------------------------
load(system.file("extdata/", "freeman_reduced_model.rda", package = "afex"))
emmeans::emm_options(lmer.df = "asymptotic")

## ---------------------------------------------------------------------------------------
mrt

## ----fig.width=7, fig.height=3.5--------------------------------------------------------
afex_plot(mrt, x = "stimulus", trace = "frequency", panel = "task") 

## ----fig.width=7, fig.height=3.5--------------------------------------------------------
plot_grid( 
  afex_plot(mrt, x = "stimulus", trace = "frequency", panel = "task", 
            id = "id"), 
  afex_plot(mrt, x = "stimulus", trace = "frequency", panel = "task", 
            id = "item"), 
  labels = c("ID", "Item") 
)

## ----fig.width=7, fig.height=3.5--------------------------------------------------------
plot_grid( 
  afex_plot(mrt, x = "stimulus", trace = "frequency", panel = "task", 
            id = "item", dodge = 0.8,
            data_geom = geom_violin, 
            data_arg = list(width = 0.5)), 
  afex_plot(mrt, x = "stimulus", trace = "frequency", panel = "task", 
            id = "item", dodge = 0.8,
            data_geom = geom_boxplot, 
            data_arg = list(width = 0.5),
            error_arg = list(size = 1.5, width = 0, linetype = 1))
)

## ---------------------------------------------------------------------------------------
pairs(emmeans::emmeans(mrt, c("stimulus", "frequency"), by = "task"))

## ----fig.width=7, fig.height=3.5--------------------------------------------------------
plot_grid( 
  afex_plot(mrt, x = "stimulus", trace = "frequency", panel = "task", 
            id = "id", error = "within"),
  afex_plot(mrt, x = "stimulus", trace = "frequency", panel = "task", 
            id = "item", dodge = 0.8, error = "within",
            data_geom = geom_violin, 
            data_arg = list(width = 0.5))
)

