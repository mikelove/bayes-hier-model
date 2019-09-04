k <- 4
n <- 40
theta <- rnorm(4, 0, 5)
x <- rep(1:4,each=10)
mu <- rnorm(n, theta[x], 1)
y <- rnorm(n, mu, 1)

library(rstan)
fit <- stan(file='simple.stan',
            data=list(k=k, n=n, x=x, y=y),
            iter=200)

print(fit, pars=paste0("theta[",1:k,"]"), digits=3)
stan_plot(fit, pars=paste0("theta[",1:k,"]"))
efit <- extract(fit)

plot(theta, colMeans(efit$theta)); abline(0,1)
plot(mu, colMeans(efit$mu)); abline(0,1)
plot(y, colMeans(efit$mu)); abline(0,1)
points(theta, colMeans(efit$theta), col="blue", cex=1.5, pch=20)
