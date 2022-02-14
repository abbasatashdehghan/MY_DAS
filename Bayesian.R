library("circlize")
yi <- c(0,0,0,0,0,0,1,0,0,0,0,0,0,1,0)
n <- length(yi)
theta <- seq(0.0001, 0.9999, length = 100)
alpha1 <- beta1 <- 1
y <- 0
dens <- dbeta(theta, alpha1, beta1)

plot(theta, dens, type = "l", ylim = c(0, 8))


for (i in  (1:n)){
  y <- y + yi[i]
  dens <- dbeta(theta, alpha1 + y, beta1 + i - y)
  lines(theta, dens, type = "l",col=sample(rainbow(n), replace = F), lwd=2)
}


alpha1 <- beta1 <- 1
y <- sum(yi)
alphaU <- alpha1 + y
betaU <- beta1  + n - y

dens <- dbeta(theta, alphaU, betaU)

plot(theta, dens, type = "l", lwd = 2)
modtheta <- (alphaU - 1)/(alphaU + betaU - 2)
lines(rep(modtheta, 4), c(0, 0.2, 0.5, 0.8))

lower <- qbeta(p = 0.025, alphaU, betaU)
upper <- qbeta(p = 0.975, alphaU, betaU)
lines(c(lower, upper), rep(0, 2))


set.seed(432)
nsamp <- 10000
draws <- rbeta(nsamp, alphaU, betaU)
hist(draws, breaks = seq(0,1, by =0.01), probability = T, col = "orange")

lower95 <- quantile(x = draws, probs = 0.025)
upper95 <- quantile(x = draws, probs = 0.975)

lower50 <- quantile(x = draws, probs = 0.25)
upper50 <- quantile(x = draws, probs = 0.75)

med <- median(draws)
lines(c(lower95, upper95), c(0,0), lwd = 2, col = "red")
lines(c(lower50, upper50), c(0, 0), lwd = 4, col = "black")
points(med, 0, pch = 16, cex = 1, col = "steelblue")



1 - pbeta(0.5, 3, 14)


pbeta(0.6, 3, 14) - pbeta(0.4, 3, 14)



draws <- rbeta(100000, 3, 14)
psi <- draws >= 0.5
mean(psi) * 100

phi <- draws >= 0.4 & draws <= 0.6
mean(phi)

mu <- 0
sigma <- c(0.1, 0.5, 1, 1.5, 2, 2.2, 2.5, 3, 4)
ptheta <- function(theta, mu, sigma){
  dnorm(log(theta/(1-theta)), mu, sigma)/(theta*(1-theta))
}

theta <- seq(0.002, 0.998, length = 500)

for (i in (1:length(sigma))){
  dens <- ptheta(theta, mu, sigma[i])
  phidraws <- rnorm(10000, mu, sigma[i])
  thetadraws <- exp(phidraws)/(1+ exp(phidraws))
  hist(thetadraws, prob = T)
  lines(theta, dens)
}
