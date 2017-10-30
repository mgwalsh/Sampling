# Geographically balanced sampling setup for MobileSurvey survey in Nigeria
# M. Walsh, October 2017

# install.packages(c("downloader","rgdal","raster","BalancedSampling"), dependencies=T)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(BalancedSampling)
})

# Data setup --------------------------------------------------------------
# Create a data folder in your current working directory
dir.create("NG_sample", showWarnings=F)
setwd("./NG_sample")

# Download & stack cropland probabilty & distance to "known roads" grids
download("https://www.dropbox.com/s/zitu29lxkf6y64l/NG_sample_grids.zip?raw=1", "NG_sample_grids.zip", mode="wb")
unzip("NG_sample_grids.zip", overwrite=T)
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
n <- N/8*0.05    ## Set sample size (number of sampling locations)
p <- rep(n/N,N)  ## Inclusion probabilities

# draw geographically balanced sample
set.seed(6405)                      ## sets reapeatable randomization seed
B <- cbind(p, rmask[,1], rmask[,2]) ## specifies balancing variables
S <- cbind(rmask[,1], rmask[,2])    ## specifies spreading variables
rsamp <- lcube(p, S, B)             ## samples from population
