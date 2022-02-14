library("clipr")
n1 <- 9
y1 <- 9
n2 <- 10
y2 <- 6
set.seed(234)
nsamp <- 100000
alpha1 <- alpha2 <- beta1 <- beta2 <- 1

#Draw from p(theta1|y1)
theta1 <- rbeta(nsamp, alpha1+y1, beta1+n1-y1)

#Draw from p(theta2|y2)
theta2 <- rbeta(nsamp, alpha2+y2, beta2+n2-y2)

delta <- theta1 - theta2

lor <- log(theta1/(1-theta1)) - log(theta2/(1-theta2))
quantdelta <- quantile(delta, c(0.025, 0.25, 0.5, 0.75, 0.975))

hist(delta, nclass = 40)
lines(quantdelta[c(1,5)], rep(0,2), lwd = 4, col = "orange")
lines(quantdelta[c(2,4)], rep(0,2), lwd = 4, col = "red")
points(quantdelta[3], 0, cex = 2, col = "black", pch = 16)


library(MASS)
mu <- c(0,0)
sigma <- c(4,4)
rho <- c(0.5, 0.7, 0.9)
nsamp <- 10000
for (i in 1:length(rho)){
  Sigma <- matrix(c(sigma[1], rho[i]*sqrt(prod(sigma)), 
                    rho[i]*sqrt(prod(sigma)), sigma[2]),2,2)
  phi <- mvrnorm(nsamp, mu, Sigma)
  theta <- matrix(0, nsamp, 2)
  theta[, 1] <- exp(phi[,1])/( 1 + exp(phi[,1]))
  theta[, 2] <- exp(phi[,2])/( 1 + exp(phi[,2]))
  plot(theta, main = rho[i])
}



theta1 <- rbeta(nsamp, 1.4+4, 8.6+14-4)
hist(theta1, nclass = 100, probability = T, xlim = c(0,1), ylim = c(0, 5),
     col = "steelblue")
lines(density(theta1), col = "red") 


