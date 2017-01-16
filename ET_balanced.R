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
download("https://www.dropbox.com/s/aspy8j9bxt97hmt/BBSRC_ROI.zip?dl=0", "BBSRC_ROI.zip", mode="wb")
unzip("BBSRC_ROI.zip", overwrite=T)
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
N <- nrow(mask)  	## Population size (in 250 m pixels)
n <- 500  	    	## Set sample size
p <- rep(n/N,N)  	## Vector of inclusion probabilities

# draw geographically balanced sample
set.seed(12358)                   ## sets reapeatable randomization seed
X <- cbind(p, mask[,1], mask[,2]) ## identifies population
rsamp <- cube(p, X)               ## samples from population

# plot result
plot(roi)
points(rmask[rsamp,1], rmask[rsamp,2], pch=3, col="red", cex=1)	
																																																																																										