#############################################################
## Analysis of the dataset 'rec' from the package 'astsa'
############################################################# 
install.packages("astsa")
library(astsa)

## What is this dataset, and what does it look like?
?rec
rec
par(mfrow = c(1,1))
plot(rec)

## Compute and store the sample mean
xbar<-mean(rec)
xbar

## Plot the ACF and PACF to get a preliminary estimate on the ARMA order
par(mfrow = c(1,2))
acf(rec)
pacf(rec)

## Compute and store the sample covariance at lags 0,1,2; these are 
## needed to compute the Yule-Walker estimators of the variance and 
## AR coefficients

ACVF<-acf(rec, type = 'covariance', plot = FALSE)
ACVF
gamma.hat<-acf(rec, type = 'covariance')$acf[1:3]
gamma.hat<-ACVF$acf[1:3]
gamma.hat

## Define the matrix and vector involved in the Yule-Walker equations
## that can be solved for the AR coefficients
A<-matrix(c(gamma.hat[1], gamma.hat[2], gamma.hat[2], gamma.hat[1]), 2,2)
A

b<-c(gamma.hat[2], gamma.hat[3])
b

phi.hat <- solve(A,b)
phi.hat

## Compute the variance of the underlying white noise process
gamma.hat
gamma.hat[-1]

sigma.sq <- gamma.hat[1] - sum(phi.hat*gamma.hat[-1])
sigma.sq       # This is the variance of the white noise
sqrt(sigma.sq) # This is the standard deviation of the white noise

# (Same computation for the variance, written out more explicitly)
gamma.hat[1] - phi.hat[1]*gamma.hat[2] - phi.hat[2]*gamma.hat[3]

#############################################
## Method of Moments Fitted model:
## (X_t - 62.263) = 1.3315(X_{t-1} -62.263) - 0.4445(X_{t-2} - 62.263) + W_t,
## white noise variance = 94.171
############################################


## Fit an AR(2) model using MLE
arima(rec, order = c(2,0,0))
?arima
## with model diagnostics
Z<-sarima(rec, 2,0,0)

##
resid<-Z$fit$residuals
sigma.sq.MLE<-var(resid)
sigma.sq.MLE
sqrt(sigma.sq.MLE)
par(mfrow = c(1,1))

## sigma.sq.MLE is the variance of the underlying white noise
## process for the fitted model, which is determined by the 
## residuals

# Let's get a quick visual check that the variance of the residuals
# is close to what we expect. 

plot(resid)
abline(h = sqrt(sigma.sq.MLE));  
abline(h = -sqrt(sigma.sq.MLE));
abline(h = 2*sqrt(sigma.sq.MLE));  
abline(h = -2*sqrt(sigma.sq.MLE));
# The inner horizontal lines should contain ~68% of the residuals;
# the outer horizontal lines should contain ~95% of the residuals.


# We can also do the same thing with a histogram, where it's also easier to
# see that the residuals are at least roughly normally distributed.

hist(resid)
?hist
approx.number.of.breaks = 30
Hist<-hist(resid, breaks = approx.number.of.breaks, xlim = c(-50, 50), ylim = c(0, 120))

abline(v = sqrt(sigma.sq.MLE))
abline(v = -sqrt(sigma.sq.MLE))
Hist$counts
sum(Hist$counts[8:11])/length(resid) # After manually counting to get 8:11 
# output is ~0.76, a little higher than 0.68, as expected, since the four
# bins we counted extend slightly outside the '1 sigma' range.

abline(v = 2*sqrt(sigma.sq.MLE))
abline(v = -2*sqrt(sigma.sq.MLE))
sum(Hist$counts[6:13])/length(resid)




############################################
library(forecast)
auto.arima(rec, trace = TRUE, stationary = TRUE, seasonal = FALSE)
# Note: We set stationary = TRUE and seasonal = FALSE because we're still in 
# week 6 of the course and nonstationary, nonseasonal models don't appear 
# until week 7.  We'll learn soon how to decide whether to modify these choices.

auto.arima(rec, trace = TRUE)
?auto.arima
# USE 'trace = TRUE' 

# ARIMA(2,0,2)            with non-zero mean : 3333.861
# ARIMA(0,0,0)            with non-zero mean : 4306.82
# ARIMA(1,0,0)            with non-zero mean : 3436.308
# ARIMA(0,0,1)            with non-zero mean : 3832.418
# ARIMA(0,0,0)            with zero mean     : 5113.725
# ARIMA(1,0,2)            with non-zero mean : 3342.182
# ARIMA(2,0,1)            with non-zero mean : 3331.821  ***** #3
# ARIMA(1,0,1)            with non-zero mean : 3351.823
# ARIMA(2,0,0)            with non-zero mean : 3330.635  ***** #1
# ARIMA(3,0,0)            with non-zero mean : 3332.87   ***** #2
# ARIMA(3,0,1)            with non-zero mean : 3334.893
# ARIMA(2,0,0)            with zero mean     : 3364.13

X.AR2.model <- sarima(rec, 2,0,0)
X.AR3.model <- sarima(rec, 3,0,0) #phi3 has high p-value of being zero
X.ARMA21.model <- sarima(rec, 2,0,1) #theta1 has high p-value of being zero

## Note: p-value of the coefficients refers to the null
## hypothesis that the coefficient is actually zero.
## High p-values mean that you should consider the 
## possibility of setting that coefficient to zero.
##
## On the other hand, p-value in LB test (at lag N) refers 
## to the null hypothesis of no correlation among the 
## first N lags.  High p-values are good in this case!


## Use 'arima' call rather than 'sarima' if you want to pass to 'forecast'.
## 'sarima' doesn't store all the information that 'forecast needs'
X.AR2.model <- arima(rec, order = c(2,0,0))
X.AR3.model <- arima(rec, order = c(3,0,0))
X.ARMA21.model <- arima(rec, order = c(2,0,1))

###########################################################

X.AR2.forecast <- forecast(X.AR2.model)
X.AR3.forecast <- forecast(X.AR3.model)
X.ARMA21.forecast <- forecast(X.ARMA21.model)

plot(X.AR2.forecast, xlim = c(1980, 1990))
plot(X.AR3.forecast, xlim = c(1980, 1990))
plot(X.ARMA21.forecast, xlim = c(1980, 1990))




