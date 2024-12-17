p <- c(1, -1.8, 1.05,-0.196)
p.roots <- polyroot(p)
p.roots


q <- c(-0.196, 1.05, -1.8, 1)
polyroot(q)


phi.tilde.zinv <- c(0.56, -1.5, 1)
roots.left <- polyroot(phi.tilde.zinv)
theta.tilde.zinv <- c(-0.4, -0.3, 1)
roots.right <- polyroot(theta.tilde.zinv)
roots.left
roots.right



n = 80
set.seed(314)
WN <-rnorm(n)
AR1pos <-arima.sim(model = list(ar = 0.85), n, innov = WN)
AR1neg <-arima.sim(model = list(ar = -0.85), n, innov = WN)
par(mfrow = c(1,2))
plot(AR1neg, xlab = '', ylim = c(-6,6))
plot(AR1pos, xlab = '', ylim = c(-6,6))


phi.tilde <- c(1, -1.1, 0.07, 0.147)
roots.left <- polyroot(phi.tilde)
theta.tilde <- c(1, 0.5, -.84)
roots.right <- polyroot(theta.tilde)
roots.left
roots.right



phi.tilde.zinv <- c(0.147, 0.07, -1.1, 1)
roots.left.zinv <- polyroot(phi.tilde.zinv)
theta.tilde.zinv <- c(-0.84, 0.5, 1)
roots.right.zinv <- polyroot(theta.tilde.zinv)
roots.left.zinv
roots.right.zinv

