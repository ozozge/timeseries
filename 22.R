#Plots x, a periodic signal f with white noise added.
# Also plots y, a filtered version of x, using R's filter function.
# y1 is the same as y, but defined from scratch.

n = 200

# Cosine Function
f = NULL
for(i in 1:n){
  f[i] = 5*cos(i/20)}
# White Noise
set.seed(1)
WN <- rnorm(n,0,1)

# x is signal (cosine) plus noise
x<- f + WN

# y is a filtered version of x
y<-filter(x, c(.2, .2, .2, .2, .2), method = "convolution", sides = 2, circular = FALSE)

# Plot x and y together
plot(x)
lines(y, lwd = 1.5)
# Defining the filtered version from scratch
y1 = NULL

for(i in 1:n){
  y1[i] = 0.2*x[i+2]+0.2*x[i+1]+0.2*x[i]+0.2*x[i-1]+0.2*x[i-2] }
plot(x)
lines(ts(y1), lwd = 1.5)



######
n = 200
set.seed(1)
WN <- rnorm(n)
theta <- c(0.5,0.5)
MA2 <- arima.sim(n, model = list(ma = theta), innov = WN)
acf(MA2)
