#######################################################
## This file generates one-step-ahead prediction coefficients
## for a specified ARMA process, using the Durbin-Levinson
## algorithm.  It also simulates that process and computes 
## the actual one-step-ahead predictions and prediction errors
## for that simulation.
#######################################################


# Choose the number of timesteps for your simulation,
# and the ARMA coefficients

n <- 1000
phi <- c(.3, 0.4)
th <- 0.7

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
m=5
phi.matrix[1:m,1:m]

# Read off the PACF from the diagonal of your 
# one-step prediction coefficients from Durbin-Levinson
PACF = NULL
for(h in 1:n){
  PACF[h] = phi.matrix[h,h]
}
PACF[1:m]

# Check the output against ARMAacf
ARMAacf(ar = phi, ma = th, lag.max = m, pacf = TRUE)

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