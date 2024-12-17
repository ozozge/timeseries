
n <- 1000
phi <- c(0.4, -0.8,0.2)
th <-c(1.3,-0.3)

# Generate the ACF of the process, starting with lag 1
ACF <- ARMAacf(ar = phi, ma = th, lag.max = n)[-1]

# Create a matrix with entries 'NA', then fill the first 
# entry with the ACF at lag 1
phi.matrix <-matrix(NA, n,n)
phi.matrix[1,1] = ACF[1]

# The Durbin-Levinson recursion for computing phi_{n,j}, 
# j=1,...n.  Only the entries on or below the diagonal 
# will be filled.
for(j in 2:n){
  num <- ACF[j] - sum(phi.matrix[j-1,(j-1):1]*ACF[1:j-1])
  denom <- 1 - sum(phi.matrix[j-1,1:(j-1)]*ACF[1:(j-1)])
  phi.matrix[j,j] <- num/denom
  for(k in 1:j-1){
    term.1 <- phi.matrix[j-1,k]
    term.2 <- phi.matrix[j,j]*phi.matrix[j-1,j-k]
    phi.matrix[j,k] <- term.1 - term.2
  }
}

# Choose how many entries of your matrix you want to see
m=3
phi.matrix[1:m,1:m]

# Read off the PACF from the diagonal of your 
# one-step prediction coefficients from Durbin-Levinson
PACF = NULL
for(h in 1:n){
  PACF[h] = phi.matrix[h,h]
}
PACF[1:m]

ARMAacf(ar = phi, ma = th, lag.max = m, pacf = TRUE)
j=3
phi.matrix[j,1:j]
###
MA4.PACF <- ARMAacf(ma = c(0.4, -0.8,0.2, 1.3,-0.3), lag.max = 30, pacf = TRUE)

# Simulate the process for which you just computed the 
# one-step-predictor coefficients.  Adjust the standard
# deviation of the white noise if desired. 
set.seed(1)
X<-arima.sim(model = list(ar = phi, ma = th), n, sd = 1)

# Generate your one-step-ahead predictions Y
Y=NULL
Y[1] = 0
for(j in 1:n-1){
  Y[j+1]=sum(phi.matrix[j,1:j]*X[j:1])
}

# Plot your simulation and the one-step-ahead predictions.
# Adjust xlim if you want to see more of the output.
par(mfrow = c(1,2))
plot.ts(X, xlim=c(0,12), lwd = 1.5, main = 'Series and 
     One-Step Predictions')
lines(ts(Y), col = 'blue', type = 'b')

# Plot the one-step prediction errors.
Z<-X-Y
plot.ts(Z, xlim = c(0,100), main = 'One-Step Prediction Errors')
abline(h=0)

Z[1:10-1]
Z[1:(10-1)]


#arima(rec, order = c(2,0,0))

acf_result<-acf(X, type = 'covariance')$acf[1:3]

data <- arima.sim(n = 1000, model = list(ar = 0.4, ma = 0.3), sd = sqrt(0.7))
acf_result <- acf(data, plot = FALSE)
variance <- acf_result$acf[1, 1, 1]



# Load the required library
library(forecast)

# Simulate AR(3) process using the given parameters
set.seed(123)
data <- arima.sim(n = 1000, model = list(ar = c(0.3, -0.5, -0.3)), sd = sqrt(0.7))

# Fit the ARIMA(3,0,0) model to the simulated data
fit <- arima(ar3_model, order = c(3, 0, 0))

# Print the model summary
summary(fit)

# The variance of the white noise process (W_t) is given by the sigma^2 of the residuals
variance_Wt <- fit$sigma2
print(variance_Wt)


# Load necessary library
library(forecast)

# Simulate the AR(3) process using the given coefficients
set.seed(123)  # For reproducibility
n <- 1000  # Number of data points
ar3_model <- arima.sim(n = n, model = list(ar = c(0.3, -0.5, -0.3)), sd = sqrt(0.4))  # AR(3) process

# Fit the AR(3) model to the simulated data
fit <- arima(ar3_model, order = c(3, 0, 0))  # AR(3) model

# Print model summary
summary(fit)

# The variance of the white noise process (W_t) is given by the sigma^2 of the residuals
variance_Wt <- fit$sigma2
print(variance_Wt)

###
# Install and load the required package
install.packages("astsa")  # If you don't have it already
library(astsa)

# Define AR(3) coefficients and the variance of the white noise
phi <- c(0.3, -0.5, -0.3)  # AR(3) coefficients
sigma2 <- 0.4  # Variance of the white noise process

# Calculate the ACF using armaacf function
acf_result <- ARMAacf(ar = phi, ma = 0, lag.max = 1000)



# Print the ACF result
print(acf_result)

# The variance (autocovariance at lag 0) is the first value of the ACF
gamma_0 <- acf_result[1]
print(paste("Gamma(0) = ", gamma_0))



