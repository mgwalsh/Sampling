# Uniform distribution in a circle

area <- 100 # plot area in square meters
n <- 1000
r <- sqrt(runif(n))*sqrt(area/pi)
t <- runif(n, 0, 2*pi)
x <- r * cos(t)
y <- r * sin(t)
plot(x, y, cex=0.6)