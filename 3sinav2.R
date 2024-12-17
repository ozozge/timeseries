#xt equals, normal sira
phi <- c(0.4, -0.13)
phi.zinv <- c(-phi[2], -phi[1], 1)
r.inv <- polyroot(phi.zinv)
A <- matrix(c(1, r.inv[1], 1, r.inv[2]), 2, 2)
b <- c(1, phi[1]/(1-phi[2]))
c <- solve(A, b)
rho.X <- NULL
for(h in 1:30){
  rho.X[h] = c[1]*r.inv[1]^h + c[2]*r.inv[2]^h}


###
#x bi taraf W bi taraf, sondan basla
# Define the AR (phi) polynomial coefficients
phi.coefficients <- c(-0.11, -1) # AR coefficients (order is reversed)

# Find the roots of the AR characteristic equation
ar.roots <- polyroot(phi.coefficients)

# Print the AR roots
cat("Roots of the AR polynomial:\n")
print(ar.roots)

# Define the MA (theta) polynomial coefficients
theta.coefficients <- c(-0.0242, -0.077,1.41,-2.3) # MA coefficients (order is reversed)

# Find the roots of the MA characteristic equation
ma.roots <- polyroot(theta.coefficients)

# Print the MA roots
cat("Roots of the MA polynomial:\n")
print(ma.roots)