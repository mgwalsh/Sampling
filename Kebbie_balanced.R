# Geographically balanced sampling setup for MobileSurvey survey in Kebbie State, Nigeria
# M. Walsh, November 2017

# install.packages(c("downloader","rgdal","raster","BalancedSampling"), dependencies=T)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(BalancedSampling)
})

# Data setup --------------------------------------------------------------
# Create a data folder in your current working directory
dir.create("Kebbie", showWarnings=F)
setwd("./Kebbie")

# Download & stack cropland probabilty & distance to "known roads" grids
download("https://www.dropbox.com/s/ugxod8j19750p0n/Kebbie_sample_grids.zip?raw=1", "Kebbie_sample_grids.zip", mode="wb")
unzip("Kebbie_sample_grids.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# Sample setup ------------------------------------------------------------
# create a ROI image based on cropland probability and distance to nearest known roads
cpt <- 1    ## set cropland probability threshold (0-1)
rdt <- 2.5  ## set maximum distance to the nearest "known road" (in km)
roi <- overlay(grids, fun=function(x) 
{return(ifelse(x[1] >= cpt && x[2] > 0 && x[2] <= rdt, 1, 0))})
plot(roi, axes=F)

# extract ROI coordinates
coord <- coordinates(roi)
index <- extract(roi, coord)
index <- as.data.frame(cbind(coord, index))
rmask <- index[which(index$index == 1),]

# Geographically balanced sampling ----------------------------------------
# set sampling parameters
N <- nrow(rmask) ## Population size (in 250 m pixels)
n <- round(N/8*0.01,0) ## Set sample size (number of sampling locations)
p <- rep(n/N,N)  ## Inclusion probabilities

# draw geographically balanced sample
set.seed(6405)                      ## sets repeatable randomization seed
B <- cbind(p, rmask[,1], rmask[,2]) ## specifies balancing variables
rsamp <- cube(p, B)                 ## cube samples from population

# plot sample result
plot(roi, axes=F)
points(rmask[rsamp,1], rmask[rsamp,2], pch=3, col="red", cex=1)

# Write files -------------------------------------------------------------
# extract sample coordinates
x <- rmask[rsamp,1]
y <- rmask[rsamp,2]
xy <- data.frame(cbind(x,y))

# generate grid / GPS waypoint ID's
res.pixel <- 1000 ## set GID resolution in meters
xgid <- ceiling(abs(xy$x)/res.pixel)
ygid <- ceiling(abs(xy$y)/res.pixel)
gidx <- ifelse(xy$x<0, paste("W", xgid, sep=""), paste("E", xgid, sep=""))
gidy <- ifelse(xy$y<0, paste("S", ygid, sep=""), paste("N", ygid, sep=""))
GID <- paste(gidx, gidy, sep="-")
xy <- cbind(xy, GID)

# project sample coordinates to longlat
coordinates(xy) <- ~x+y
crs(xy) <- "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"
KB_locs_LL <- as.data.frame(spTransform(xy, CRS("+proj=longlat +datum=WGS84")))
colnames(KB_locs_LL)[1:3] <- c("GID","Lon","Lat")

# write files
write.csv(KB_locs_LL, "KB_locs.csv", row.names = F) ## csv file
gpx <- SpatialPointsDataFrame(coords = KB_locs_LL[,c(2,3)], data = KB_locs_LL, proj4string = CRS("+proj=longlat + ellps=WGS84")) 
plot(gpx, axes = T)
