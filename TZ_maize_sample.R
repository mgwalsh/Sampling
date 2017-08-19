#' Geographically balanced sampling setup for soil nutrient survey of Tanzania
#' M. Walsh, February 2017

# install.packages(c("downloader","rgdal","raster","BalancedSampling"), dependencies=T)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(BalancedSampling)
})

# Data setup --------------------------------------------------------------
# Create a data folder in your current working directory
dir.create("TZ_sample", showWarnings=F)
setwd("./TZ_sample")

# Download & stack maize probabilty & distance to "known roads" grids
download("https://www.dropbox.com/s/83qwr9stamkrqfi/TZ_sample_grids.zip?dl=0", "TZ_sample_grids.zip", mode="wb")
unzip("TZ_sample_grids.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# Download MobileSurvey data
download("https://www.dropbox.com/s/hrbyf6s9o6z7yww/MZ_syst_080117.csv?dl=0", "MZ_syst_080117.csv", mode="wb")
mob <- read.table("MZ_syst_080117.csv", header=T, sep=",")
mzp <- mob[which(mob$MZP=='Y'),] ## select MobileSurvey locations where maize is present

# Sample setup ------------------------------------------------------------
# create a ROI image based on cropland probability and distance to nearest known roads
cpt <- 0.8 ## set cropland probability threshold (0-1)
rdt <- 2.5 ## set maximum distance to the nearest "known road" (in km)
roi <- overlay(grids, fun=function(x){ 
               return(ifelse(x[1] >= cpt && x[2] > 0 && x[2] <= rdt, 1, 0))})
plot(roi, axes=F, legend=F)

# Georeference MobileSurvey data and extract gridded variables
mzp.proj <- as.data.frame(project(cbind(mzp$lon, mzp$lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(mzp.proj) <- c("x","y") ## laea coordinates
mzp <- cbind(mzp, mzp.proj) ## bind laea coordinates to mob dataframe
coordinates(mzp) <- ~x+y ## create spatial points object
projection(mzp) <- projection(grids) ## set coordinate reference system
mzproi <- extract(roi, mzp) ## extract gridded roi indicator @ point locations
mzproi <- as.data.frame(mzproi) ## convert back to a dataframe
mzp <- cbind.data.frame(mzp, mzproi) ## bind points with roi indicator in a dataframe
mzp <- unique(na.omit(mzp)) ## dataframe includes only unique & complete records
mzp <- mzp[which(mzp$mzproi==1),] ## select maize locations within roi

# Geographically balanced sampling ----------------------------------------
# set sampling parameters
N <- nrow(mzp)  ## Population size
n <- 1000       ## Set sample size (number of sampling locations)
p <- rep(n/N,N) ## Inclusion probabilities

# draw geographically balanced sample
set.seed(6405)                    ## sets reapeatable randomization seed
B <- cbind(p, mzp[,9], mzp[,10])  ## specifies balancing variables
S <- cbind(mzp[,9], mzp[,10])     ## specifies spreading variables
rsamp <- lcube(p, S, B)           ## samples from population

# plot sample result
plot(roi, axes=F, legend=F)
points(mzp[rsamp,9], mzp[rsamp,10], pch=3, col="red", cex=1)

# Write files -------------------------------------------------------------
# extract sample coordinates
x <- mzp[rsamp,9]
y <- mzp[rsamp,10]
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
TZ_locs_LL <- as.data.frame(spTransform(xy, CRS("+proj=longlat +datum=WGS84")))
colnames(TZ_locs_LL)[1:3] <- c("GID","Lon","Lat")

# write files
write.csv(TZ_locs_LL, "TZ_locs.csv", row.names = F) ## csv file
gpx <- SpatialPointsDataFrame(coords = TZ_locs_LL[,c(2,3)], data = TZ_locs_LL, proj4string = CRS("+proj=longlat + ellps=WGS84")) 
plot(gpx, axes=T)
