
##################################################################
##                2-factor Within-Subject Design                ##
##################################################################

data(md_12.1)
aw <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

## basic functionality
afex_plot(aw, x = ~angle, trace = ~noise)

afex_plot(aw, x = "noise", trace = "angle")

## adjust jitter on y-axis for raw data in background
## use within-subject confidence intervals
p1 <- afex_plot(aw, x = "noise", trace = "angle", 
                error = "within-SE", error_exp = 1.96,
                data_jitter_y = 5)
p1

## use different themes for nicer graphs:
p1 + ggplot2::theme_classic()
p1 + ggplot2::theme_light()
p1 + ggplot2::theme_minimal()
\dontrun{
p1 + jtools::theme_apa()
p1 + ggpubr::theme_pubr()
}

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
          data_arg = list(alpha = 0.3, color = "darkgrey"),
          data_jitter_y = 0.1, 
          emmeans_arg = list(model = "multivariate")) +
  ggplot2::theme_light()

# with color instead of linetype to separate trace factor
afex_plot(a1, ~hour, ~treatment, ~phase, 
          mapping_trace = c("shape", "color"),
          dodge = 0.65, 
          data_arg = list(alpha = 0.5),
          data_jitter_y = 0.1, 
          emmeans_arg = list(model = "multivariate")) +
  ggplot2::theme_light()


# only color to separate trace factor
afex_plot(a1, ~hour, ~treatment, ~phase, 
          mapping_trace = c("color"), 
          point_arg = list(shape = 15),
          dodge = 0.65, 
          data_arg = list(alpha = 0.5, shape = 15),
          data_jitter_y = 0.1, 
          emmeans_arg = list(model = "multivariate")) +
  ggplot2::theme_light()


## plot involving all 4 factors:
afex_plot(a1, ~hour, ~treatment, ~gender+phase, 
          emmeans_arg = list(model = "multivariate"), 
          dodge = 0.6, data_arg = list(alpha = 0.4, color = "darkgrey")) +
  ggplot2::theme_light()


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
                    error = "mean-SE", return = "data")$means$error,
  within = afex_plot(a1, ~phase, ~hour, 
                     error = "within-SE", return = "data")$means$error,
  between = afex_plot(a1, ~phase, ~hour, 
                      error = "between-SE", return = "data")$means$error)
## mixed design
cbind(
  afex_plot(a1, ~phase, ~treatment, 
            error = "model", return = "data")$means[,c("phase", "treatment", "y", "SE")],
  multivariate = afex_plot(a1, ~phase, ~treatment, 
                           emmeans_arg = list(model = "multivariate"),
                           error = "model", return = "data")$means$error,
  mean = afex_plot(a1, ~phase, ~treatment, 
                    error = "mean-SE", return = "data")$means$error,
  within = afex_plot(a1, ~phase, ~treatment, 
                     error = "within-SE", return = "data")$means$error,
  between = afex_plot(a1, ~phase, ~treatment, 
                      error = "between-SE", return = "data")$means$error)
