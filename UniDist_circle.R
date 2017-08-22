# Uniform distribution in a circle of a given area

area <- 100 # area in square meters
n <- 1000
r <- sqrt(runif(n))*sqrt(area/pi)
t <- runif(n, 0, 2*pi)
x <- r * cos(t)
y <- r * sin(t)
plot(x, y, pch=3, cex=0.6)
