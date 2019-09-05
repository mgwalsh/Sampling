# Spatially balanced oversample for GeoSurvey building density estimates of Zambia
# M. Walsh, September 2019

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
dir.create("ZM_GSL2_sample", showWarnings=F)
setwd("./ZM_GSL2_sample")

# download GADM-L2 shapefile (courtesy: http://www.gadm.org)
download("https://www.dropbox.com/s/3x870g2n5cjge16/ZM_GADM_L2.zip?raw=1", "ZM_GADM_L2.zip", mode="wb")
unzip("ZM_GADM_L2.zip", overwrite=T)
shape <- shapefile("gadm36_ZMB_2.shp")

# download GS Level-1 building mask prediction
download("https://www.dropbox.com/s/jhyo5h9lozl9sen/ZM_GSL1_preds.zip?raw=1", "ZM_GSL1_preds.zip", mode="wb")
unzip("ZM_GSL1_preds.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# Sample setup ------------------------------------------------------------
# create a ROI image
cpt <- 1    ## set building mask to 1
roi <- overlay(grids$BM19, fun=function(x) {return(ifelse(x[1] >= cpt, 1, 0))})
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
set.seed(35813)                     ## sets repeatable randomization seed
B <- cbind(p, rmask[,1], rmask[,2]) ## specifies balancing variables
rsamp <- cube(p, B)                 ## samples from population

# plot sample result
plot(roi, axes=F, legend=F)
points(rmask[rsamp,1], rmask[rsamp,2], pch=3, col="red", cex=0.2)

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
colnames(samp) <- c("Province", "District", "lon", "lat")
write.csv(samp, "ZM_GSL2_sample.csv", row.names = F)

# Sampling map widget -----------------------------------------------------
# render map
w <- leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(samp$lon, samp$lat, clusterOptions = markerClusterOptions())
w ## plot widget 
saveWidget(w, 'ZM_GSL2_sample.html', selfcontained = T)

