# Uniform distribution in a circle

n <- 1000
r <- sqrt(runif(n))
t <- runif(n, 0, 2*pi)
x <- r * cos(t)
y <- r * sin(t)
plot(x, y, cex=0.6)