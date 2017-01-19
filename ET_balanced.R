#' Geographically balanced sampling setup for GeoNutrition survey in Ethiopia
#' M. Lark & M. Walsh, January 2017

# install.packages(c("downloader","rgdal","raster","BalancedSampling"), dependencies=T)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(BalancedSampling)
})

# Data setup --------------------------------------------------------------
# Create a data folder in your current working directory
dir.create("BBSRC_sample", showWarnings=F)
setwd("./BBSRC_sample")

# Download & stack cropland probabilty & distance to "known roads" grids
download("https://www.dropbox.com/s/m44c7ylcsn96cxi/BBSRC_samp_grids_500m.zip?dl=0", "BBSRC_samp_grids_500m.zip", mode="wb")
unzip("BBSRC_samp_grids_500m.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# Sample setup ------------------------------------------------------------
# create a ROI image based on cropland probability and distance to nearest known roads
cpt <- 0.90 ## set cropland probability threshold (0-1)
rdt <- 2.5  ## set maximum distance to the nearest "known road" (in km)
roi <- overlay(grids, fun=function(x) 
              {return(ifelse(x[1] >= cpt && x[2] > 0 && x[2] <= rdt, 1, 0))})
plot(roi)

# extract ROI coordinates
coord <- coordinates(roi)
index <- extract(roi, coord)
index <- as.data.frame(cbind(coord, index))
rmask <- index[which(index$index == 1),]

# specify ROI extent
ext <- data.frame(lat = c(10,10,12,12), lon = c(37,40,37,40)) ## set ROI extent in degrees
names(ext) <- c("lat","lon")
coordinates(ext) <- ~ lon + lat
proj4string(ext) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
laea <- spTransform(ext, CRS("+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))

# subregion bounding box
minx <- laea@bbox[1,1]
maxx <- laea@bbox[1,2]
miny <- laea@bbox[2,1]
maxy <- laea@bbox[2,2]
sreg <- (rmask[,1] <= maxx) & (rmask[,1] >= minx) &
        (rmask[,2] <= maxy) & (rmask[,2] >= miny)
mask <- rmask[sreg,]

# Geographically balanced sampling ----------------------------------------
# set sampling parameters
N <- nrow(mask) ## Population size (in 500 m pixels)
n <- 500        ## Set sample size (number of sampling locations)
p <- rep(n/N,N) ## Inclusion probabilities

# draw geographically balanced sample
set.seed(6405)                    ## sets reapeatable randomization seed
B <- cbind(p, mask[,1], mask[,2]) ## specifies balancing variables
S <- cbind(mask[,1], mask[,2])    ## specifies spreading variables
rsamp <- lcube(p, S, B)           ## samples from population

# plot sample result
plot(roi)
points(mask[rsamp,1], mask[rsamp,2], pch=3, col="red", cex=1)

# Write files -------------------------------------------------------------
# extract sample coordinates
x <- mask[rsamp,1]
y <- mask[rsamp,2]
xy <- data.frame(cbind(x,y))

# generate grid / GPS waypoint ID's
res.pixel <- 1000 ## set GID resolution in meters
xgid <- ceiling(abs(xy$x)/res.pixel)
ygid <- ceiling(abs(xy$y)/res.pixel)
gidx <- ifelse(xy$x<0, paste("W", xgid, sep=""), paste("E", xgid, sep=""))
gidy <- ifelse(xy$y<0, paste("S", ygid, sep=""), paste("N", ygid, sep=""))
GID <- paste(gidx, gidy, sep="-")
xy <- cbind(xy, GID)

# plot results
coordinates(xy) <- ~x+y
crs(xy) <- "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"
plot(crop(roi, xy))         ## plot cropped ROI grid
plot(xy, col="red", add=T)  ## overlay sample points

# project sample coordinates to longlat
ET_locs_LL <- as.data.frame(spTransform(xy, CRS("+proj=longlat +datum=WGS84")))
colnames(ET_locs_LL)[1:3] <- c("GID","Lon","Lat")

# write files
write.csv(ET_locs_LL, "ET_locs.csv", row.names = F) ## csv file
gpx <- SpatialPointsDataFrame(coords = ET_locs_LL[,c(2,3)], data = ET_locs_LL, proj4string = CRS("+proj=longlat + ellps=WGS84")) 
plot(gpx, axes = T)

# needs fixing !!!
# writeOGR(gpx, dsn = "ET_locs.gpx", dataset_options = "GPX_USE_EXTENSIONS = yes",
#         layer = "waypoints", driver = "GPX", overwrite_layer = T) ## GPX file
																																																																																										