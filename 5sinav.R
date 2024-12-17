
set.seed(1)
n <- 10000
X <- arima.sim(model = list(ar = c(-0.3, 0.4)), n)   #x=

#par(mfrow = c(1,2))
#acf(X)
#pacf(X)

rho.hat<-acf(X, plot = F)$acf[-1][1:2]
rho.hat
gamma.0 <- acf(X, type = 'covariance', plot = F)$acf[1]

rho.hat<-c(0.6,0.5)
R.hat <- matrix(c(1, 0.6, 0.5, 1),2,2) 
phi.hat <- solve(R.hat, rho.hat)   #bu



R <- matrix(c(1, rho.hat[1], rho.hat[1], 1),2,2)
phi.hat <- solve(R,rho.hat)
phi.hat


var.W <- gamma.0*(1 - sum(rho.hat*phi.hat))
var.W

R <- matrix(c(1, 0.6, 0.5, 1),2,2)





#
# Given autocorrelations
rho_1 <- 0.6
rho_2 <- 0.5

# Construct the matrix for the Yule-Walker equations
acf_values <- c(rho_1, rho_2)

# Set up the matrix equation A * [phi_1, phi_2] = acf_values
A <- matrix(c(1, rho_1, rho_1, rho_2), nrow = 2, byrow = TRUE)

# Solve the system of equations to find phi_1 and phi_2
phi_values <- solve(A, acf_values)

# Display the results
phi_values


###
# Load necessary package
library(forecast)

# Assuming 'BJsales' is the time series dataset
# First, create the differenced dataset
diff_BJsales <- diff(BJsales)

# Fit the ARMA(1,1) with non-zero mean
fit_arma11_nonzero <- arima(diff_BJsales, order = c(1, 0, 1), include.mean = TRUE)

# Fit the AR(2) with non-zero mean
fit_ar2_nonzero <- arima(diff_BJsales, order = c(2, 0, 0), include.mean = TRUE)

# Fit the AR(2) with zero mean
fit_ar2_zero <- arima(diff_BJsales, order = c(2, 0, 0), include.mean = FALSE)

# Fit the ARMA(1,1) with zero mean
fit_arma11_zero <- arima(diff_BJsales, order = c(1, 0, 1), include.mean = FALSE)

# Extract and compare the residual variances (white noise variances)
residual_variances <- c(
  fit_arma11_nonzero$sigma2,
  fit_ar2_nonzero$sigma2,
  fit_ar2_zero$sigma2,
  fit_arma11_zero$sigma2
)

# Print out the residual variances
names(residual_variances) <- c("ARMA(1,1) with non-zero mean", "AR(2) with non-zero mean", 
                               "AR(2) with zero mean", "ARMA(1,1) with zero mean")
print(residual_variances)

# Identify the model with the lowest residual variance
best_model <- names(residual_variances)[which.min(residual_variances)]
cat("The model with the lowest associated white-noise variance is:", best_model)

###
# Load necessary package
library(forecast)

# Assuming 'BJsales' is the time series dataset
# First, create the differenced dataset
diff_BJsales <- diff(BJsales)

# Fit the AR(2) model to the differenced dataset with mean zero
fit_ar2_zero <- arima(diff_BJsales, order = c(2, 0, 0), include.mean = FALSE)

# Print the model summary
summary(fit_ar2_zero)

# Extract coefficients
coefficients <- fit_ar2_zero$coef
print(coefficients)

###
# Given autocovariances
gamma_0 <- 3
gamma_1 <- 1.5
gamma_2 <- -0.5
gamma_3 <- 0.3

# Set up the autocovariance matrix
Gamma <- matrix(c(gamma_0, gamma_1, gamma_2,
                  gamma_1, gamma_0, gamma_1,
                  gamma_2, gamma_1, gamma_0), 
                nrow = 3, byrow = TRUE)

# Set up the vector for the autocorrelations (excluding gamma_0)
gamma_vector <- c(gamma_1, gamma_2, gamma_3)

# Solve the system of equations to get the AR coefficients
phi <- solve(Gamma, gamma_vector)

# Display the estimated AR coefficients
phi


###
# Given autocovariances
gamma_0 <- 1.2
gamma_1 <- 1.1
gamma_2 <- 2
#gamma_3 <- 0.3

# Set up the autocovariance matrix for AR(3)
Gamma <- matrix(c(gamma_0, gamma_1, gamma_2,
                  gamma_1, gamma_0, gamma_1,
                  gamma_2, gamma_1, gamma_0), 
                nrow = 3, byrow = TRUE)

# Set up the vector for the autocovariances (excluding gamma_0)
gamma_vector <- c(gamma_1, gamma_2, gamma_3)

# Solve the system to get the AR coefficients (phi)
phi <- solve(Gamma, gamma_vector)

# Now calculate the variance of white noise (sigma^2)
sigma_squared <- (gamma_0 - sum(phi * c(gamma_1, gamma_2, gamma_3))) / 1

# Print the result
round(sigma_squared, 3)

###
# Given autocovariances
gamma_0 <- 1.2
gamma_1 <- 1.1
gamma_2 <- 1

# Set up the autocovariance matrix for AR(2)
Gamma <- matrix(c(gamma_0, gamma_1,
                  gamma_1, gamma_0), 
                nrow = 2, byrow = TRUE)

# Set up the vector for the autocovariances (excluding gamma_0)
gamma_vector <- c(gamma_1, gamma_2)

# Solve the system to get the AR coefficients (phi)
phi <- solve(Gamma, gamma_vector)

# Now calculate the variance of white noise (sigma^2)
sigma_squared <- (gamma_0 - sum(phi * c(gamma_1, gamma_2))) / 1

# Print the result
round(sigma_squared, 3)

####
# Assuming you have the 'BJsales' data and have already differenced it:
diff_BJsales <- diff(BJsales)

# Fit ARMA(1,1) model with nonzero mean
model <- arima(diff_BJsales, order = c(1, 0, 1))

# Show the model output
summary(model)




