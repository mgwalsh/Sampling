#' Nigeria nutrient ommission trial sampling locations based on 1MQ GeoSurvey reference points
#' M.Walsh, April 2015

#+ Required packages
# install.packages(c("downloader","raster","rgdal")), dependencies=TRUE)
require(downloader)
require(raster)
require(rgdal)

#+ Data downloads ----------------------------------------------------------
# Create a "Data" folder in your current working directory
dir.create("NG_trials", showWarnings=F)
dat_dir <- "./NG_trials"

# download 1MQ GeoSurvey cropland presence data
download("https://www.dropbox.com/s/9y1fso9g8qks1pp/1MQ_CRP_pos.csv?dl=0", "./NG_trials/1MQ_CRP_pos.csv", mode="wb")
geosv <- read.table(paste(dat_dir, "/1MQ_CRP_pos.csv", sep=""), header=T, sep=",")

# download Nigeria IPNI ROI prediction grids and stack in raster
download("https://www.dropbox.com/s/k374cza1z0dwu65/NG_trial_grids.zip?dl=0", "./NG_trials/NG_trial_grids.zip", mode="wb")
unzip("./NG_trials/NG_trial_grids.zip", exdir=dat_dir, overwrite=T)
glist <- list.files(path=dat_dir, pattern="tif", full.names=T)
grid <- stack(glist)

#+ Data setup --------------------------------------------------------------
# Project test data to grid CRS
geosv.proj <- as.data.frame(project(cbind(geosv$Lon, geosv$Lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(geosv.proj) <- c("x","y")
geosv <- cbind(geosv, geosv.proj)
coordinates(geosv) <- ~x+y
projection(geosv) <- projection(grid)

# Extract gridded variables to test data observations
gsexv <- data.frame(coordinates(geosv), extract(grid, geosv))
gsexv <- na.omit(gsexv)
coordinates(gsexv) <- ~x+y
projection(gsexv) <- projection(grid)
NG_locs_LL <- as.data.frame(spTransform(gsexv, CRS("+proj=longlat +datum=WGS84")))
colnames(NG_locs_LL)[1:2] <- c("Lon", "Lat")

# Write potential trial locations
write.csv(NG_locs_LL, "./NG_trials/NG_locs_LL.csv")
