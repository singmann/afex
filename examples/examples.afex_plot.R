# note: use library("ggplot") to avoid "ggplot2::" in the following

##################################################################
##                2-factor Within-Subject Design                ##
##################################################################

data(md_12.1)
aw <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

##---------------------------------------------------------------
##                    Basic Interaction Plots                   -
##---------------------------------------------------------------

afex_plot(aw, x = "angle", trace = "noise") 
# or: afex_plot(aw, x = ~angle, trace = ~noise)

afex_plot(aw, x = "noise", trace = "angle")

### For within-subject designs, using within-subject CIs is better:
afex_plot(aw, x = "angle", trace = "noise", error = "within") 
(p1 <- afex_plot(aw, x = "noise", trace = "angle", error = "within"))

## use different themes for nicer graphs:
p1 + ggplot2::theme_classic()
p1 + ggplot2::theme_light()
p1 + ggplot2::theme_minimal()
\dontrun{
p1 + jtools::theme_apa()
p1 + ggpubr::theme_pubr()

### set theme globally for R session:
ggplot2::theme_set(ggplot2::theme_light())
}

### There are several ways to deal with overlapping points in the background besides alpha

# 1. using the default data geom and ggplot2::position_jitterdodge
afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.2,
          data_arg = list(
            position = 
              ggplot2::position_jitterdodge(
                jitter.width = 0, 
                jitter.height = 2, 
                dodge.width = 0.2  ## needs to be same as dodge
                ),
            color = "darkgrey"))

\dontrun{
# 2. using ggbeeswarm::geom_beeswarm
afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.5,
          data_geom = ggbeeswarm::geom_beeswarm,
          data_arg = list(
            dodge.width = 0.5,  ## needs to be same as dodge
            cex = 0.8,
            color = "darkgrey"
          ))
}

# 3. do not display points, but use a violinplot: ggplot2::geom_violin
afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.5,
          data_geom = ggplot2::geom_violin, 
          data_arg = list(width = 0.5))

# 4. violinplots with color: ggplot2::geom_violin
afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.5, 
          mapping = c("linetype", "shape", "fill"),
          data_geom = ggplot2::geom_violin, 
          data_arg = list(width = 0.5))

# 5. do not display points, but use a boxplot: ggplot2::geom_violin
afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.5,
          data_geom = ggplot2::geom_boxplot, 
          data_arg = list(width = 0.3))
          
\dontrun{
# 6. combine points with boxplot: ggpol::geom_boxjitter
## currently requires attaching ggpol explicitly:
library("ggpol")
afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.5,
          data_geom = ggpol::geom_boxjitter, 
          data_arg = list(width = 0.3))
## hides error bars!

# 7. nicer variant of ggpol::geom_boxjitter
afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.5,
          mapping = c("shape", "fill"),
          data_geom = ggpol::geom_boxjitter, 
          data_arg = list(
            width = 0.3, 
            jitter.width = 0,
            jitter.height = 10,
            outlier.intersect = TRUE),
          point_arg = list(size = 2.5), 
          error_arg = list(size = 1.5, width = 0))


# 8. nicer variant of ggpol::geom_boxjitter without lines
afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.7,
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
}


##---------------------------------------------------------------
##                      Basic One-Way Plots                     -
##---------------------------------------------------------------

afex_plot(aw, x = "angle", error = "within") ## default

## with color we need larger points
afex_plot(aw, x = "angle", mapping = "color", error = "within", 
          point_arg = list(size = 2.5), 
          error_arg = list(size = 1.5, width = 0.05)) 

\dontrun{
library("ggpol") ## currently required for combination of boxplot and points
afex_plot(aw, x = "angle", error = "within", data_geom = ggpol::geom_boxjitter)

## nicer
afex_plot(aw, x = "angle", error = "within", data_geom = ggpol::geom_boxjitter, 
          mapping = "fill", data_alpha = 0.7, 
          data_arg = list(
            width = 0.6, 
            jitter.width = 0.07,
            jitter.height = 10,
            outlier.intersect = TRUE
          ),
          point_arg = list(size = 2.5), 
          error_arg = list(size = 1.5, width = 0.05),
          error_exp = 1.96)
}


##---------------------------------------------------------------
##                      Other Basic Options                     -
##---------------------------------------------------------------

## relabel factor levels via new_levels
afex_plot(aw, x = "noise", trace = "angle", 
          new_levels = list(angle = c("0", "4", "8"),
                            noise = c("Absent", "Present")))

#################################################################
##                    4-factor Mixed Design                    ##
#################################################################

data(obk.long, package = "afex")
a1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
              data = obk.long, observed = "gender")

## too difficult to see anything
afex_plot(a1, ~phase*hour, ~treatment) +
  ggplot2::theme_light()

## better
afex_plot(a1, ~hour, ~treatment, ~phase) +
  ggplot2::theme_light()

## even better and different model-based standard errors
afex_plot(a1, ~hour, ~treatment, ~phase, 
          dodge = 0.65, 
          data_arg = list(
            position = 
              ggplot2::position_jitterdodge(
                jitter.width = 0, 
                jitter.height = 0.2, 
                dodge.width = 0.65  ## needs to be same as dodge
                ),
            color = "darkgrey"),
          emmeans_arg = list(model = "multivariate")) +
  ggplot2::theme_classic()

# with color instead of linetype to separate trace factor
afex_plot(a1, ~hour, ~treatment, ~phase, 
          mapping = c("shape", "color"),
          dodge = 0.65, 
          data_arg = list(
            position = 
              ggplot2::position_jitterdodge(
                jitter.width = 0, 
                jitter.height = 0.2, 
                dodge.width = 0.65  ## needs to be same as dodge
                )),
          emmeans_arg = list(model = "multivariate")) +
  ggplot2::theme_light()

# only color to separate trace factor
afex_plot(a1, ~hour, ~treatment, ~phase, 
          mapping = "color",
          dodge = 0.65, 
          data_arg = list(
            position = 
              ggplot2::position_jitterdodge(
                jitter.width = 0, 
                jitter.height = 0.2, 
                dodge.width = 0.65  ## needs to be same as dodge
                )),
          emmeans_arg = list(model = "multivariate")) +
  ggplot2::theme_classic()


## plot involving all 4 factors:
afex_plot(a1, ~hour, ~treatment, ~gender+phase, 
          dodge = 0.65, 
          data_arg = list(
            position = 
              ggplot2::position_jitterdodge(
                jitter.width = 0, 
                jitter.height = 0.2, 
                dodge.width = 0.65  ## needs to be same as dodge
                ),
            color = "darkgrey"),
          emmeans_arg = list(model = "multivariate")) +
  ggplot2::theme_classic()


##---------------------------------------------------------------
##              Different Standard Errors Available             -
##---------------------------------------------------------------

## purely within-design
cbind(
  afex_plot(a1, ~phase, ~hour, 
            error = "model", return = "data")$means[,c("phase", "hour", "y", "SE")],
  multivariate = afex_plot(a1, ~phase, ~hour, 
                           emmeans_arg = list(model = "multivariate"),
                           error = "model", return = "data")$means$error,
  mean = afex_plot(a1, ~phase, ~hour, 
                    error = "mean", return = "data")$means$error,
  within = afex_plot(a1, ~phase, ~hour, 
                     error = "within", return = "data")$means$error,
  between = afex_plot(a1, ~phase, ~hour, 
                      error = "between", return = "data")$means$error)
## mixed design
cbind(
  afex_plot(a1, ~phase, ~treatment, 
            error = "model", return = "data")$means[,c("phase", "treatment", "y", "SE")],
  multivariate = afex_plot(a1, ~phase, ~treatment, 
                           emmeans_arg = list(model = "multivariate"),
                           error = "model", return = "data")$means$error,
  mean = afex_plot(a1, ~phase, ~treatment, 
                    error = "mean", return = "data")$means$error,
  within = afex_plot(a1, ~phase, ~treatment, 
                     error = "within", return = "data")$means$error,
  between = afex_plot(a1, ~phase, ~treatment, 
                      error = "between", return = "data")$means$error)

##################################################################
##                         Mixed Models                         ##
##################################################################

data("Machines", package = "MEMSS") 
m1 <- mixed(score ~ Machine + (Machine|Worker), data=Machines)

pairs(emmeans::emmeans(m1, "Machine"))
# contrast   estimate       SE df t.ratio p.value
# A - B     -7.966667 2.420850  5  -3.291  0.0481
# A - C    -13.916667 1.540100  5  -9.036  0.0007
# B - C     -5.950000 2.446475  5  -2.432  0.1253

## Default (i.e., model-based) error bars suggest no difference between Machines.
## This contrasts with pairwise comparisons above.
afex_plot(m1, "Machine")

## Impression from within-subject error bars is more in line with pattern of differences.
afex_plot(m1, "Machine", error = "within")


\dontrun{
data("fhch2010") # load 
fhch <- droplevels(fhch2010[ fhch2010$correct,]) # remove errors
### following model should take less than a minute to fit:
mrt <- mixed(log_rt ~ task*stimulus*frequency + (stimulus*frequency||id)+
               (task||item), fhch, method = "S", expand_re = TRUE)

## way too many points in background:
afex_plot(mrt, "stimulus", "frequency", "task") 

## better to restrict plot of data to one random-effects grouping variable
afex_plot(mrt, "stimulus", "frequency", "task", random = "id")
## when plotting data from a single random effect, different error bars are possible:
afex_plot(mrt, "stimulus", "frequency", "task", random = "id", error = "within")
afex_plot(mrt, "stimulus", "frequency", "task", random = "id", error = "mean")

## compare visual impression with:
pairs(emmeans::emmeans(mrt, c("stimulus", "frequency"), by = "task"))

## same logic also possible for other random-effects grouping factor
afex_plot(mrt, "stimulus", "frequency", "task", random = "item")
## within-item error bars are misleading here. task is sole within-items factor.
afex_plot(mrt, "stimulus", "frequency", "task", random = "item", error = "within")
## CIs based on stanard error of mean look small, but not unreasonable given results.
afex_plot(mrt, "stimulus", "frequency", "task", random = "item", error = "mean")

### compare distribution of individual data for different random effects:
## requires package cowplot
p_id <- afex_plot(mrt, "stimulus", "frequency", "task", random = "id", 
                  error = "within", dodge = 0.7,
                  data_geom = ggplot2::geom_violin, 
                  mapping = c("shape", "fill"),
                  data_arg = list(width = 0.7)) +
  ggplot2::scale_shape_manual(values = c(4, 17)) +
  ggplot2::labs(title = "ID")

p_item <- afex_plot(mrt, "stimulus", "frequency", "task", random = "item", 
          error = "within", dodge = 0.7,
          data_geom = ggplot2::geom_violin, 
          mapping = c("shape", "fill"),
          data_arg = list(width = 0.7)) +
  ggplot2::scale_shape_manual(values = c(4, 17)) +
  ggplot2::labs(title = "Item")

### see: https://cran.r-project.org/package=cowplot/vignettes/shared_legends.html
p_comb <- cowplot::plot_grid(
  p_id + ggplot2::theme_light() + ggplot2::theme(legend.position="none"),
  p_item + ggplot2::theme_light() + ggplot2::theme(legend.position="none")
  )
legend <- cowplot::get_legend(p_id + ggplot2::theme(legend.position="bottom"))
cowplot::plot_grid(p_comb, legend, 
                   ncol = 1, 
                   rel_heights = c(1, 0.1))
}

##----------------------------------------------------------------
##                    Support for lme4::lmer                     -
##----------------------------------------------------------------

Oats <- nlme::Oats
## afex_plot does currently not support implicit nesting: (1|Block/Variety)
## Instead, we need to create the factor explicitly
Oats$VarBlock <- Oats$Variety:Oats$Block
Oats.lmer <- lmer(yield ~ Variety * factor(nitro) + (1|VarBlock) + (1|Block),
                        data = Oats)
afex_plot(Oats.lmer, "nitro", "Variety")
afex_plot(Oats.lmer, "nitro", panel = "Variety")
