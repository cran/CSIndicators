## ------------------------------------------------------------------------
library(CSIndicators)
set.seed(1)
oldpar <- par(no.readonly = TRUE) 
wind <- rweibull(n = 1000, shape = 2, scale = 6)
WPD <- WindPowerDensity(wind)
mean(WPD)
sd(WPD)
par(mfrow=c(1, 2))
hist(wind, breaks = seq(0, 20))
hist(WPD, breaks = seq(0, 4000, 200))

## ------------------------------------------------------------------------
WPD <- WindPowerDensity(wind, ro = 1.15)

## ------------------------------------------------------------------------
WCFI <- WindCapacityFactor(wind, IEC_class = "I")
WCFIII <- WindCapacityFactor(wind, IEC_class = "III")
par(mfrow=c(1, 3))
hist(wind, breaks = seq(0, 20))
hist(WCFI, breaks = seq(0, 1, 0.05), ylim = c(0, 500))
hist(WCFIII, breaks = seq(0, 1, 0.05), ylim = c(0, 500))
par(oldpar) 

