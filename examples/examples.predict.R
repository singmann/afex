
data(obk.long, package = "afex")

# estimate mixed ANOVA on the full design:
fit <- aov_ez("id", "value", obk.long, between = c("treatment", "gender"), 
              within = c("phase", "hour"), observed = "gender")

new_data <- expand.grid(
  treatment = "A",
  gender = "F",
  phase = c("pre", "post"),
  hour = c(1, 5)
)

predict(fit, newdata = new_data)
predict(fit, newdata = new_data, append = TRUE)