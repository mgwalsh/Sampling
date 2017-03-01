#' Geographically balanced sampling setup for maize nutrient survey in Tanzania
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
mzpgrd <- extract(roi, mzp) ## extract gridded roi indicator @ point locations
mzpgrd <- as.data.frame(mzpgrd) ## convert back to a dataframe
mzp <- cbind.data.frame(mzp, mzpgrd) ## bind points with roi indicator in a dataframe
mzp <- unique(na.omit(mzp)) ## dataframe includes only unique & complete records of maize locations

