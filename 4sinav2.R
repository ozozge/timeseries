# Install and load the required package
install.packages("forecast")  # Run this if you don't have it installed
library(forecast)

# Define the AR(2) model coefficients
phi1 <- 0.4
phi2 <- 0.3

# Simulate the AR(2) process (with some random white noise)
set.seed(123)  # For reproducibility
n <- 100  # Length of the series
ar2_model <- arima.sim(n = n, model = list(ar = c(phi1, phi2)), sd = sqrt(0.7))  # Simulate the AR(2) process

# Fit the AR(2) model to the simulated data
fit <- arima(ar2_model, order = c(2, 0, 0))  # AR(2) model

# Print the summary of the fitted model
summary(fit)

# The variance of the white noise process (W_t) is given by the sigma^2 of the residuals
variance_Wt <- fit$sigma2
print(paste("Variance of the white noise process (W_t): ", variance_Wt))
