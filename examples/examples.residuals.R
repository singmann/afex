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

### in case data is correctly ordered before fitting, this warning is not shown

## between data:
obk2 <- aggregate(value ~ gender + treatment + id , data = obk.long, FUN = mean)
between2 <- aov_car(value ~ treatment*gender + Error(id), data = obk2)

residuals(between2) ## no warning
all.equal(obk2, between2$data$long[,colnames(obk2)]) ## TRUE

# Therefore okay:
obk2$residuals <- residuals(between2)

## within data
obk3 <- obk.long[with(obk.long, order(id, phase, hour)), ]
within2 <- aov_car(value ~ 1 + Error(id/(phase*hour)), data = obk3)
residuals(within2) ## no warning, because order is correct
# Therefore okay:
obk3$residuals <- residuals(within2)

## Same for fitted values:
# (show warnings)
fitted(within)
fitted(mixed)
fitted(between)

## Get fitted values plus data used for fitting: 
fitted(within, append = TRUE)
fitted(mixed, append = TRUE)
fitted(between, append = TRUE)

## No warnings:
fitted(between2)
fitted(within2)

#### residuals() and fitted() methods can be used for plotting
### requires package ggResidpanel
if (require("ggResidpanel")) {
  resid_auxpanel(residuals = residuals(mixed), predicted = fitted(mixed))
}
