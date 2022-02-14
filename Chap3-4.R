theta <- seq(0,1, 0.01)
dens <- theta^3 * (1-theta)^13 + 10 * theta^4 * (1-theta)^12 + 
  45 * theta^5 * (1-theta)^11
plot(theta, dens, type = "l", ylim = c(0, 1.1*max(dens)),
     xaxs = "i",yaxs = "i", cex = 2)

theta <- seq(0,1,0.01)
dens <- dbeta(theta, 17,5)
plot(theta, dens, type = "l", ylim = c(0, 1.1*max(dens)))
pbeta(0.7, 17, 5) - pbeta(0.5, 17, 5)
#using sampling rather than distribution
nsamp <- 1000000
theta <- rbeta(nsamp, 17, 5)
mean(theta  > 0.5 & theta < 0.7)
mean(theta)
median(theta)
lower95 <- quantile(theta, probs = 0.025)
upper95 <- quantile(theta, probs = 0.975)
c(lower95, upper95)

c(qbeta(0.025, 17, 5), qbeta(0.975, 17, 5))


#################################
theta <- seq(0.0001, 0.9999, length=500)
postD <- 0.5 * dbeta(theta, 31, 71) + 0.5 * dbeta(theta, 71, 31)
plot(theta, postD, type="l", ylab= "density",
     main="Mixture density 0.5 Beta(31, 71) + 0.5 Beta(71, 31)")

posty <- theta^7 * (1-theta)^3 * postD
posty <- posty / mean(posty)
plot(theta, posty, type="l", ylab="density",
     main="Posteriors of theta using p(theta|D) prior and Un(0,1) prior")
lines(theta, dbeta(theta, 8, 4), lty=3)
