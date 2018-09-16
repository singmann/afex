
data(obk.long, package = "afex")

# estimate mixed ANOVA on the full design:
a1 <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
              data = obk.long, observed = "gender")

afex_plot(a1, ~phase*hour, ~treatment, 
          alpha_data = 0.5, jitter_y = 0.3, dodge = 0.75) +
  ggplot2::theme_light()


afex_plot(a1, ~phase, ~hour, 
          alpha_data = 0.5, jitter_y = 0.3, dodge = 0.75) +
  ggplot2::theme_light()

afex_plot(a1, ~phase, ~hour, error = "SE", 
          alpha_data = 0.5, jitter_y = 0.3, dodge = 0.75) +
  ggplot2::theme_light()

afex_plot(a1, ~phase, ~hour, error = "CMO", 
          alpha_data = 0.5, jitter_y = 0.3, dodge = 0.75) +
  ggplot2::theme_light()

afex_plot(a1, ~phase*hour, error_width = 0.5)
