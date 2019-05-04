
### Setup ANOVAs
data(obk.long, package = "afex")
between_1 <- aov_car(value ~ treatment + Error(id), data = obk.long)
between_2 <- aov_car(value ~ treatment*gender + Error(id), data = obk.long)
mixed <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)
within <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk.long)

### Levene Test for Homogeneity of Variances
test_levene(between_1)
test_levene(between_2)
test_levene(mixed)
\dontrun{
test_levene(within) ## fails
}

### Mauchly Test of Sphericity
\dontrun{
## fails for between-subjects only models:
test_sphericity(between_1)
test_sphericity(between_2)
}
test_sphericity(mixed)
test_sphericity(within)

