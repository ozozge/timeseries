set.seed(1) 
WN <- rnorm(200,0,3) 
plot.ts(WN, xlab = '', ylab = '', main = 'Gaussian White Noise')

RW <- cumsum(WN) 
plot.ts(RW, xlab = '', ylab = '', main = 'Random Walk')


set.seed(1)
WN <- rnorm(200,0,3)
plot.ts(WN, xlab = '', ylab = '', main = 'Gaussian White Noise')


RW <- cumsum(WN)
plot.ts(RW, xlab = '', ylab = '', main = 'Random Walk')

OR

RW = NULL
RW[1] = WN[1]
for(i in 2:200){
  RW[i] = RW[i-1] + WN[i]}
plot.ts(RW, xlab = '', ylab = '', main = 'Random Walk')

##
MA1 <- arima.sim(model = list(ma = 0.7), n = 200, innov = WN)
plot.ts(MA1, xlab = '', ylab = '', main = 'An MA(1) process')

OR

MA1 = NULL
for(i in 1:200){
  MA1[i] = WN[i] + 0.7*WN[i-1]}
plot.ts(MA1, xlab = '', ylab = '',main = 'An MA(1) process')


