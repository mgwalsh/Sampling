# Spatially balanced sampling setup for GeoSurvey of Tanganyika & Haut-Lomami regions, DRC
# M. Walsh, October 2018

# install.packages(c("downloader","rgdal","raster","BalancedSampling","leaflet","htmlwidgets"), dependencies=T)
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
dir.create("DRC_GS_sample", showWarnings=F)
setwd("./DRC_GS_sample")

# download GADM-L2 shapefile (courtesy: http://www.gadm.org)
download("https://www.dropbox.com/s/ufvu0a6oou1xwhh/DRC_GADM_L2.zip?raw=1", "DRC_GADM_L2.zip", mode="wb")
unzip("DRC_GADM_L2.zip", overwrite=T)
shape <- shapefile("gadm36_COD_2.shp")

# download ROI mask
download("https://www.dropbox.com/s/b5jjwljh0otnbvy/ROI.zip?raw=1", "ROI.zip", mode="wb")
unzip("ROI.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# Sample setup ------------------------------------------------------------
# create a ROI image based on ROI mask
cpt <- 1    ## set land mask to 1
roi <- overlay(grids, fun=function(x) {return(ifelse(x[1] >= cpt, 1, 0))})
plot(roi, axes=F, legend=F)

# extract ROI coordinates
coord <- coordinates(roi)
index <- extract(roi, coord)
index <- as.data.frame(cbind(coord, index))
rmask <- index[which(index$index == 1),]

# Geographically balanced sampling ----------------------------------------
# set sampling parameters
N <- nrow(rmask) ## population size (in 250 m pixels)
n <- round(N/16*0.1,0) ## set sample size (number of sampling locations)
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

# attach GADM-L2 and above unit names from shape
coordinates(xy) <- ~x+y
crs(xy) <- "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"
sloc <- spTransform(xy, CRS(proj4string(shape)))
gadm <- sloc %over% shape
sloc <- as.data.frame(sloc)
samp <- cbind(gadm[ ,c(4,7)], sloc)
colnames(samp) <- c("region", "territory", "lon", "lat")
write.csv(samp, "DRC_GS_sample.csv", row.names = F)

# Sampling map widget -----------------------------------------------------
# render map
w <- leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(samp$lon, samp$lat, clusterOptions = markerClusterOptions())
w ## plot widget 

# save widget
saveWidget(w, 'DRC_GS_sample.html', selfcontained = T)
