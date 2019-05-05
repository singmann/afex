### Setup ANOVAs
data(obk.long, package = "afex")
between <- aov_car(value ~ treatment*gender + Error(id), data = obk.long)
within <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk.long)
mixed <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)

# different output for different types of residuals
residuals(within, model = "multivariate")
residuals(within, model = "univariate")

# Mixed residuals also have "id" level residuals in univariate residuals
residuals(mixed, model = "multivariate")
residuals(mixed, model = "univariate")

# multivariate and univariate residuals are the same for between-ANOVA only
residuals(between, model = "multivariate")
residuals(between, model = "univariate")

# Plot
residuals_qqplot(within)
residuals_qqplot(mixed)
residuals_qqplot(between)
