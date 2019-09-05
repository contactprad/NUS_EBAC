install.packages("astsa")
install.packages("ggplot")
library("astsa")
library("ggplot2")
#Plot the data
par(mfrow = c(2,1)) # set up the graphics
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment")

# write.csv(soi, "D:/04_Self_Practice/soi.csv")
# write.csv(rec, "D:/04_Self_Practice/rec.csv")

# ts.plot(x, y, gpars = list(col = c("black", "red")))

#Acf and ccf for soi and rec
par(mfrow=c(3,1))
acf(soi, 60, main="Southern Oscillation Index")
acf(rec, 60, main="Recruitment")
ccf(soi, rec, 60, main="SOI vs Recruitment", ylab="CCF")

# The cross-correlation function peaks at h = -????6, showing
# that the SOI measured at time t-????6 months is associated with the Recruitment
# series at time t. We could say the SOI leads the Recruitment series by
# six months. The sign of the ACF is negative, leading to the conclusion that
# the two series move in different directions; that is, increases in SOI lead to
# decreases in Recruitment and vice versa. Again, note the periodicity of 12
# months in the CCF.
###########################################################
# Below is lagged regression for rec against soi with lag of 6 month.. soi can be used to
# predict fish population after 6 months from said time
# Performing lagged regression in R is a little difficult because the series
# must be aligned prior to running the regression. The easiest way to do this
# is to create a data frame that we call fish using ts.intersect, which aligns
# the lagged series.

fish = ts.intersect(rec, soiL6=lag(soi,-6), dframe=TRUE)
summary(lm(rec~soiL6, data=fish, na.action=NULL))
lag1.plot(soi, 12) # Fig 2.7
lag2.plot(soi, rec, 8)

acf2(soi)
(fit = arima(soi, xreg=time(soi), order=c(1, 0, 0)))
ar1 = as.numeric(fit$coef[1]) # = 0.5875387
soi.pw = resid(fit)
rec.d = resid(lm(rec~time(rec), na.action=NULL))
rec.fil = filter(rec.d, filter=c(1, -ar1), method="conv", sides=1)
ccf(soi.pw, rec.fil, main="", ylab="CCF", na.action=na.omit)
#ccf(rec.fil,soi.pw,  main="", ylab="CCF", na.action=na.omit)

# In the code above, soi.pw is the prewhitened detrended SOI series, rec.d
# is the detrended Recruitment series, and rec.fil is the filtered, detrended
# Recruitment series. In the ccf calculation, na.action=na.omit is used because
# rec.fil[1] is NA.

rec.d = resid(lm(rec~time(rec), na.action=NULL))
soi.d = resid(lm(soi~time(soi), na.action=NULL))
fish = ts.intersect(rec.d, rec.d1=lag(rec.d,-1), soi.d5=lag(soi,-5),
                    dframe=TRUE)
summary(fish.fit <- lm(rec.d~0+rec.d1+soi.d5, data=fish))
om1 = as.numeric(fish.fit$coef[1])
eta.hat = filter(resid(fish.fit), filter=c(1,-om1), method="recur",
                 sides=1)
acf2(eta.hat)
(eta.fit <- arima(eta.hat, order=c(3,0,0)))

