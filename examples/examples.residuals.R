### Setup ANOVAs
data(obk.long, package = "afex")
between <- aov_car(value ~ treatment*gender + Error(id), data = obk.long)
within <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk.long)
mixed <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)

# Residuals
residuals(within)
residuals(mixed)
residuals(between)
residuals(between, return_df = TRUE) # since data was aggragated across within conditions

# Plot
residuals_qqplot(within)
residuals_qqplot(mixed)
residuals_qqplot(between)
