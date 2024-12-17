# Parameters
phi <- 0.4
sigma_squared <- 2

# Calculate gamma_X(0)
gamma_X_0 <- sigma_squared / (1 - phi^2)

# Print the result rounded to two decimal places
round(gamma_X_0, 2)

##
# Parameters
theta <- 0.6
sigma_squared <- 4
h <- 3

# Calculate gamma_X(h)
gamma_X_h <- ifelse(h > 1, 0, ifelse(h == 1, theta * sigma_squared, sigma_squared * (1 + theta^2)))

# Print the result
gamma_X_h

##
# Parameters
phi <- 0.7
theta <- 0.4

# Compute psi coefficients
psi_0 <- 1
psi_1 <- theta + phi * psi_0
psi_2 <- phi * psi_1

# Print psi_2
psi_2

##
# Parameters
theta <- 0.8
h <- 3

# Calculate rho_X(h)
rho_X_h <- ifelse(h > 1, 0, ifelse(h == 1, theta / (1 + theta^2), 1))

# Print the result
rho_X_h

##
# Parameters
theta1 <- 0.5
theta2 <- -0.2
sigma_squared <- 1
h <- 3

# Compute gamma_X(h)
gamma_X_h <- ifelse(h == 0, sigma_squared * (1 + theta1^2 + theta2^2),
                    ifelse(h == 1, sigma_squared * (theta1 + theta1 * theta2),
                           ifelse(h == 2, sigma_squared * theta2, 0)))

# Print result
gamma_X_h

##
# Parameters
phi <- 0.7
theta <- 0.4

# Compute psi coefficients
psi_0 <- 1
psi_1 <- theta + phi * psi_0
psi_2 <- phi * psi_1
psi_3 <- phi * psi_2

# Print psi_3
psi_3

##
# Parameters
theta <- 0.6
sigma_squared <- 4
h <- 1

# Compute gamma_X(h)
gamma_X_h <- ifelse(h == 0, sigma_squared * (1 + theta^2),
                    ifelse(h == 1, theta * sigma_squared, 0))

# Print result
gamma_X_h


polyroot(c(-0.3,-0.1,1))


phi <- c(-1.3, -0.36)
phi.zinv <- c(-phi[2], -phi[1], 1)
r.inv <- polyroot(phi.zinv)
A <- matrix(c(1, r.inv[1], 1, r.inv[2]), 2, 2)
b <- c(1, phi[1]/(1-phi[2]))
c <- solve(A, b)
rho.X <- NULL
for(h in 1:30){
  rho.X[h] = c[1]*r.inv[1]^h + c[2]*r.inv[2]^h}

phi <- c(0.1, 0.3)
phi.zinv <- c(-phi[2], -phi[1], 1)
r.inv <- polyroot(phi.zinv)
A <- matrix(c(1, r.inv[1], 1, r.inv[2]), 2, 2)
b <- c(1, phi[1]/(1-phi[2]))
c <- solve(A, b)
rho.X <- NULL
for(h in 1:30){
  rho.X[h] = c[1]*r.inv[1]^h + c[2]*r.inv[2]^h}

phi <- c(0,0.64)
phi.zinv <- c(-phi[2], -phi[1], 1)
r.inv <- polyroot(phi.zinv)
A <- matrix(c(1, r.inv[1], 1, r.inv[2]), 2, 2)
b <- c(1, phi[1]/(1-phi[2]))
c <- solve(A, b)
rho.X <- NULL
for(h in 1:30){
  rho.X[h] = c[1]*r.inv[1]^h + c[2]*r.inv[2]^h}

###

# Define the AR (phi) polynomial coefficients
phi.coefficients <- c(0.5733, -2.716, 4.74, -3.6, 1) # AR coefficients (order is reversed)

# Find the roots of the AR characteristic equation
ar.roots <- polyroot(phi.coefficients)

# Print the AR roots
cat("Roots of the AR polynomial:\n")
print(ar.roots)

# Define the MA (theta) polynomial coefficients
theta.coefficients <- c(0.56, -1.5, 1) # MA coefficients (order is reversed)

# Find the roots of the MA characteristic equation
ma.roots <- polyroot(theta.coefficients)

# Print the MA roots
cat("Roots of the MA polynomial:\n")
print(ma.roots)

###
# Define the AR (phi) polynomial coefficients
phi.coefficients <- c(-0.11,1, 1) # AR coefficients (order is reversed)

# Find the roots of the AR characteristic equation
ar.roots <- polyroot(phi.coefficients)

# Print the AR roots
cat("Roots of the AR polynomial:\n")
print(ar.roots)

# Define the MA (theta) polynomial coefficients
theta.coefficients <- c(-0.0242, -0.077,1.41,-2.3, 1) # MA coefficients (order is reversed)

# Find the roots of the MA characteristic equation
ma.roots <- polyroot(theta.coefficients)

# Print the MA roots
cat("Roots of the MA polynomial:\n")
print(ma.roots)


