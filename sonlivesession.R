###############################################################################
## Live Session 3 Code                                                      ###
## Analysis of dataset "ozone.dat" from tsdl                                ###
## Available at http://robjhyndman.com/tsdldata/monthly/ozone.dat           ###
##                                                                          ###
## Note: This is intended to help you with Manually Graded Assessment #3    ###
## and the Final Summative Assessment.  But you will generally need to      ###
## provide more detail in your write-up than is given in the comments below.###
## Furthermore, some of the code below is not explained thoroughly in the   ###
## comments.  It will be discussed during Live Session 3.                   ###
###############################################################################

install.packages("pacman")
library(pacman)
p_load(astsa, fUnitRoots, forecast)

?scan

ozone.data <- scan("http://robjhyndman.com/tsdldata/monthly/ozone.dat",skip=1)
ozone.data
ozone.ts <- ts(ozone.data, start = c(1955,1), end = c(1972,12), frequency = 12)
ozone.ts

# Don't forget to format properly!
# Here's an improperly formatted call, which will end 
# in *January* 1972:
# ozone.ts <- ts(ozone.data, start = 1955, end = 1972, frequency = 12)
# ozone.ts
#


par(mfrow = c(1,1))
plot(ozone.ts)
#




# Perform a lag-12 difference to remove the periodicity
X<-diff(ozone.ts, lag = 12)
X
plot(X)
mean(X)
abline(h = 0)
#




# Run augmented Dickey--Fuller Test 
(length(X)-1)^(1/3)
adfTest(X, 'c', lag = 5)
# Small p-value --> reject null hypothesis/do not difference again



# Look at ACF and PACF of the lag-12 differenced data
par(mfrow = c(1,2))
acf(X, lag.max = 48)
pacf(X, lag.max = 48)
#



# Seasonal MA order appears to be at least 1 (possibly just 1); 
# Nonseasonal MA order appears to be at least 1.  
# Unclear from plots whether autoregressive parts are nontrivial.



auto.arima(ozone.ts, trace = TRUE, approximation = FALSE)

# ARIMA(2,0,2)(1,1,1)[12] with drift         : 497.3933
# ARIMA(0,0,0)(0,1,0)[12] with drift         : 577.6264
# ARIMA(1,0,0)(1,1,0)[12] with drift         : 519.5568
# ARIMA(0,0,1)(0,1,1)[12] with drift         : 499.2161
# ARIMA(0,0,0)(0,1,0)[12]                    : 580.5451
# ARIMA(2,0,2)(0,1,1)[12] with drift         : 495.3425
# ARIMA(2,0,2)(0,1,0)[12] with drift         : 564.7016
# ARIMA(2,0,2)(0,1,2)[12] with drift         : 497.3874
# ARIMA(2,0,2)(1,1,0)[12] with drift         : 521.6861
# ARIMA(2,0,2)(1,1,2)[12] with drift         : 499.5857
# ****ARIMA(1,0,2)(0,1,1)[12] with drift**** : 493.399
# ARIMA(1,0,2)(0,1,0)[12] with drift         : 563.7001
# ARIMA(1,0,2)(1,1,1)[12] with drift         : 495.3963
# ARIMA(1,0,2)(0,1,2)[12] with drift         : 495.389
# ARIMA(1,0,2)(1,1,0)[12] with drift         : 520.0505
# ARIMA(1,0,2)(1,1,2)[12] with drift         : 497.5634
# ARIMA(0,0,2)(0,1,1)[12] with drift         : 499.481
# ****ARIMA(1,0,1)(0,1,1)[12] with drift**** : 494.1814
# ARIMA(1,0,3)(0,1,1)[12] with drift         : 495.049
# ARIMA(0,0,3)(0,1,1)[12] with drift         : 500.6113
# ARIMA(2,0,1)(0,1,1)[12] with drift         : 494.1077
# ARIMA(2,0,3)(0,1,1)[12] with drift         : 496.8872
# ARIMA(1,0,2)(0,1,1)[12]                    : 495.5194


# picks   ARIMA(1,0,2)(0,1,1)[12] with drift
# Close AICC and fewer parameters: 
#         ARIMA(1,0,1)(0,1,1)[12] with drift
#
# Note: Without "approximation = FALSE", routine picks 
# ARIMA(3,0,3)(2,1,0)[12]
# A few other models may be worth trying; see the output.




# Let's compare with what auto.arima does for 
# our seasonally differenced data
auto.arima(X, allowdrift = FALSE, trace = TRUE, stationary = TRUE, 
           approximation = FALSE)

# Picks ARIMA(1,0,2)(0,0,1)[12] (no surprise)

# And finally, let's look at the original data, but
# setting allowdrift = FALSE
auto.arima(ozone.ts, allowdrift = FALSE, trace = TRUE,
           approximation = FALSE)
# Still picks ARIMA(1,0,2)(0,1,1)[12]


# Try ARIMA(1,0,2)(0,1,1)[12] on original data
ozone.model1<-sarima(ozone.ts, 1,0,2,0,1,1, 12)
ozone.model1$fit$aic


ozone.model1.diff<-sarima(X, 1,0,2,0,0,1, 12)
# Model for lag-12 differenced dataset is
# (1 - 0.8842 B)(Y_t + 0.1475)
#  = (1 - 0.5565 B - 0.1527 B^2)(1 - 0.7466B^{12})W_t,
# sigma^2 = 0.64933.
#
# To get a model for the original dataset, 
# replace Y_t with (1 - B^{12})X_t.


# Try ARIMA(3,0,3)(2,1,0)[12] and try removing 
# parameters with high p-values
# using the "fixed" option
?sarima
sarima(ozone.ts, 3,0,3,2,1,0,12)
sarima(ozone.ts, 3,0,3,2,1,0,12, fixed = c(0, NA, NA, NA, NA, NA, NA, NA, NA))
test<-sarima(ozone.ts, 3,0,3,0,1,1,12, fixed = c(0, NA, NA, NA, 0, NA, NA, NA))
test$fit$aic

# This last model is another contender; it has a (very slightly) lower AICC 
# than our current favorite; however, there are 2 more parameters, so it's 
# probably not worth the marginal gain.

sarima(ozone.ts, 1,0,1, 0,1,1,12)
# This is another contender; it has comparable AICC and 1 fewer parameter.
# However, the Ljung-Box p-values are appreciably lower than our other favorites

sarima(ozone.ts, 1,0,2, 0,1,1,12, no.constant = TRUE)
# This is another option.  1 less parameter, but the diagnostics and AICC are 
# a little worse than before


# We accept the SARIMA(1,0,2)(0,1,1)[12] model.  Let's store it for use in 
# forecasts
ozone.fit <- arima(ozone.ts, order = c(1,0,2), 
                   seasonal = list(order = c(0,1,1), period = 12))

# Another set of diagnostics
checkresiduals(ozone.fit)

###################################################################
## Let's forecast! 

ozone.forecast <- forecast(ozone.fit, h = 36)

par(mfrow = c(1,1))
plot(ozone.forecast)
plot(ozone.forecast, xlim = c(1965, 1976))
plot(ozone.forecast, xlim = c(1973, 1976))
plot(ozone.forecast, xlim = c(1965, 1978))
plot(ozone.forecast, xlim = c(1973, 1978))


ozone.forecast

###################################################################
## Exponential Smoothing Fit

ozone.fit.HW <- HoltWinters(ozone.ts)
ozone.fit.HW
# Output consists of the parameters for the model
# alpha = smoothing parameter for level
# beta = smoothing parameter for trend
# gamma = smoothing parameter for seasonality
# (See Module 8, Lesson 2 for the relevant equations, both the "original" 
# system and the state-space representation.)
# a  = final value of level
# b  = final value of trend
# sj = final value of seasonal component j

plot(ozone.fit.HW)
checkresiduals(ozone.fit.HW)

# Visual inspection of the fit seems okay; diagnostics are acceptable
# (though Ljung-Box test p-value is not much higher than 0.1).

ozone.forecast.HW <- forecast(ozone.fit.HW, h = 36)


par(mfrow = c(1,1))
plot(ozone.forecast.HW)
plot(ozone.forecast.HW, xlim = c(1965, 1976))
plot(ozone.forecast.HW, xlim = c(1971, 1975))


# Just out of curiosity, compare the ARIMA and exponential smoothing forecasts:
par(mfrow = c(1,2))
plot(ozone.forecast, xlim = c(1965, 1976))
plot(ozone.forecast.HW, xlim = c(1965, 1976))


#
ozone.fit.HW$coefficients
a<-as.numeric(ozone.fit.HW$coefficients[1])
a
b<-as.numeric(ozone.fit.HW$coefficients[2])
b
s2<-as.numeric(ozone.fit.HW$coefficients[4])
s2
s5<-as.numeric(ozone.fit.HW$coefficients[7])
s5

# Point forecast equation (Module 8, Lesson 2, p. 5, bottom) is
# x^t_{t+m} = l_t + m*b_t + s_{t+m - T(k+1)}
# ozone.ts has December 1972 as final observation.  
# To get a forecast for May 1973, we set m = 5 (and use s5)
# To get a forecast for February 1974, we set m = 14 (and use s2)

a + 5*b + s5
a + 14*b + s2

ozone.forecast.HW


