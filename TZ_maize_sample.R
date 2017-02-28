#' Geographically balanced sampling setup for Maize system survey in Tanzania
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
download("https://www.dropbox.com/s/holobfn4dpjxsaq/TZ_maize%20samp_grids.zip?dl=0", "TZ_samp_grids.zip", mode="wb")
unzip("TZ_samp_grids.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# Sample setup ------------------------------------------------------------
# create a ROI image based on maize cropland probability and distance to nearest known roads
cpt <- 0.3 ## set maize probability threshold (0-1)
rdt <- 2.5 ## set maximum distance to the nearest "known road" (in km)
roi <- overlay(grids, fun=function(x){ 
               return(ifelse(x[2] >= cpt && x[1] > 0 && x[1] <= rdt, 1, 0))})
plot(roi, axes=F, legend=F)
