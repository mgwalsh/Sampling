# Geographically balanced sampling setup for MobileSurvey in Nigeria
# M. Walsh, November 2017

# install.packages(c("downloader","rgdal","raster","BalancedSampling"), dependencies=T)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(sp)
  require(BalancedSampling)
})

# Data setup --------------------------------------------------------------
# create a data folder in your current working directory
dir.create("NG_sample", showWarnings=F)
setwd("./NG_sample")

# download & stack cropland probability/mask & distance to "known roads" grids
download("https://www.dropbox.com/s/zitu29lxkf6y64l/NG_sample_grids.zip?raw=1", "NG_sample_grids.zip", mode="wb")
unzip("NG_sample_grids.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# download GADM-L2 shapefile (courtesy: http://www.gadm.org)
download("https://www.dropbox.com/s/y3h6l7yu00orm78/NGA_adm2.zip?raw=1", "NGA_adm2.zip", mode="wb")
unzip("NGA_adm2.zip", overwrite=T)
shape <- shapefile("NGA_adm2.shp")

# Sample setup ------------------------------------------------------------
# create a ROI image based on cropland probability and distance to nearest known roads
cpt <- 1    ## set cropland mask threshold (0-1)
rdt <- 2.5  ## set maximum distance to the nearest "known road" (in km)
roi <- overlay(grids, fun=function(x) 
{return(ifelse(x[1] >= cpt && x[2] > 0 && x[2] <= rdt, 1, 0))})
plot(roi, axes=F, legend=F)

# extract ROI coordinates
coord <- coordinates(roi)
index <- extract(roi, coord)
index <- as.data.frame(cbind(coord, index))
rmask <- index[which(index$index == 1),]

# Geographically balanced sampling ----------------------------------------
# set sampling parameters
N <- nrow(rmask) ## population size (in 250 m pixels)
n <- round(N/8*0.025,0) ## set sample size (number of sampling locations)
p <- rep(n/N,N)  ## inclusion probabilities

# draw geographically balanced sample
set.seed(6405)                      ## sets repeatable randomization seed
B <- cbind(p, rmask[,1], rmask[,2]) ## specifies balancing variables
rsamp <- cube(p, B)                 ## samples from population

# plot sample result
plot(roi, axes=F, legend=F)
points(rmask[rsamp,1], rmask[rsamp,2], pch=3, col="red", cex=0.3)

# Write files -------------------------------------------------------------
# extract sample coordinates
x <- rmask[rsamp,1]
y <- rmask[rsamp,2]
xy <- data.frame(cbind(x,y))

# attach State & LGA names
coordinates(xy) <- ~x+y
crs(xy) <- "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"
sloc <- spTransform(xy, CRS(proj4string(shape)))
gadm <- sloc %over% shape
sloc <- as.data.frame(sloc)
samp <- cbind(gadm[ ,c(5,7)], sloc)
colnames(samp) <- c("State", "LGA", "Lon", "Lat")

# Write file --------------------------------------------------------------
write.csv(samp, "NG_sample.csv", row.names = F)
