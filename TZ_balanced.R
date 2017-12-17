# Geographically balanced sampling setup for MobileSurvey in Tanzania
# M. Walsh, November 2017

# install.packages(c("downloader","rgdal","raster","BalancedSampling"), dependencies=T)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(sp)
  require(BalancedSampling)
  require(leaflet)
  require(htmlwidgets)
})

# Data setup --------------------------------------------------------------
# create a data folder in your current working directory
dir.create("TZ_sample", showWarnings=F)
setwd("./TZ_sample")

# download & stack cropland probability/mask & distance to "known roads" grids
download("https://www.dropbox.com/s/83qwr9stamkrqfi/TZ_sample_grids.zip?raw=1", "TZ_sample_grids.zip", mode="wb")
unzip("TZ_sample_grids.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# download GADM-L3 shapefile (courtesy: http://www.gadm.org)
download("https://www.dropbox.com/s/bhefsc8u120uqwp/TZA_adm3.zip?raw=1", "TZA_adm3.zip", mode="wb")
unzip("TZA_adm3.zip", overwrite=T)
shape <- shapefile("TZA_adm3.shp")

# Sample setup ------------------------------------------------------------
# create a ROI image based on cropland mask and distance to nearest main roads
cpt <- 1    ## set cropland mask threshold (0-1)
rdt <- 5  ## set maximum distance to the nearest "known road" (in km)
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

# attach GADM-L3 and above unit names from shape
coordinates(xy) <- ~x+y
crs(xy) <- "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"
sloc <- spTransform(xy, CRS(proj4string(shape)))
gadm <- sloc %over% shape
sloc <- as.data.frame(sloc)
samp <- cbind(gadm[ ,c(5,7,9)], sloc)
colnames(samp) <- c("Region", "District", "Ward", "Lon", "Lat")

# Write file --------------------------------------------------------------
write.csv(samp, "TZ_sample.csv", row.names = F)

# Sampling map widget -----------------------------------------------------
# render map
w <- leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(sloc$x, sloc$y, clusterOptions = markerClusterOptions())
w ## plot widget 

# save widget
saveWidget(w, 'TZ_sample.html', selfcontained = F)
