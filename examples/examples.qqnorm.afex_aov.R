
data(obk.long, package = "afex")


m1 <- aov_car(value ~ treatment * gender + Error(id / (phase * hour)),
              data = obk.long,
              observed = "gender")

qqnorm(m1, type = "marginal")
qqnorm(m1, type = "univariate")
qqnorm(m1, type = "multivariate")

if (require(qqplotr)) qqnorm(m1, type = "univariate", detrend = TRUE)
