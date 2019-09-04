set.seed(4)
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
png("post_theta.png", res=120)
stan_plot(fit, pars=paste0("theta[",1:k,"]"))
dev.off()
efit <- extract(fit)

library(rafalib)

png("theta.png")
bigpar()
plot(theta, colMeans(efit$theta), xlim=c(-5,5), ylim=c(-5,5),
     pch=4, cex=3, lwd=3, ylab="posterior means", col="blue")
abline(0,1)
dev.off()

png("mu.png", width=550, height=550)
bigpar()
plot(mu, colMeans(efit$mu),
     ylab="estimate",
     pch=20, cex=2, col=x, xlim=c(-4,7), ylim=c(-4,7))
segments(mu, y, mu, colMeans(efit$mu), col=x)
points(mu, y, col=x, cex=1.5, pch=21, bg="white")
abline(0,1)
legend("bottomright", c("MLE","Bayes"), pch=21:20, inset=.05, cex=1.5, col="grey50")
dev.off()

png("mu_err.png")
bigpar(2,1)
brks=seq(0,3,.5)
hist(abs(y - mu), breaks=brks, ylim=c(0,20),
     col="grey50", border="white", xlab="", ylab="", main="MLE abs residuals")
abline(h=1:4*5, col=rgb(0,0,0,.3))
hist(abs(colMeans(efit$mu) - mu), breaks=brks, ylim=c(0,20),
     col="grey50", border="white", xlab="", ylab="", main="Bayes abs residuals")
abline(h=1:4*5, col=rgb(0,0,0,.3))
dev.off()

plot(y, colMeans(efit$mu)); abline(0,1)
points(theta, colMeans(efit$theta), col="blue", cex=1.5, pch=20)

dat <- data.frame(k=factor(rep(x,2)),
                  estimate=c(y, colMeans(efit$mu)),
                  idx=rep(1:n, 2),
                  type=factor(rep(c("MLE","Bayes"), each=n)))
library(ggplot2)
png("shrink.png", height=400, width=1000, res=120)
ggplot(dat, aes(estimate, type, col=k, group=idx)) + geom_point() + geom_line()
dev.off()
