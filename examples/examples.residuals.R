### Setup ANOVAs
data(obk.long, package = "afex")
between <- aov_car(value ~ treatment*gender + Error(id), data = obk.long)
within <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk.long)
mixed <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), data = obk.long)

# All residuals call produce the warning that the data was changed during calculation.
residuals(within)
residuals(mixed)
residuals(between)

## Get residuals plus data used for fitting: 
residuals(within, append = TRUE)
residuals(mixed, append = TRUE)
residuals(between, append = TRUE)

## in case data is correctly ordered before fitting, this warning is not shown:
obk2 <- aggregate(value ~ gender + treatment + id , data = obk.long, FUN = mean)
between2 <- aov_car(value ~ treatment*gender + Error(id), data = obk2)

residuals(between2) ## no warning because
all.equal(obk2, between2$data$long[,colnames(obk2)]) ## TRUE
## Therefore okay:
obk2$residuals <- residuals(between2)

