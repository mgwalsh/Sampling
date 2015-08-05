#' Generate grid cell waypoints for field navigation
#' M. Walsh & J. Chen, July 2015
 
#+ Required packages
# install.packages(c("downloader","raster","rgdal")), dependencies=TRUE)
require(downloader)
require(rgdal)

#+ Data downloads ----------------------------------------------------------
# Create a "Data" folder in your current working directory
dir.create("GID", showWarnings=F)
dat_dir <- "./GID"

# download GeoSurvey sample data
download("https://www.dropbox.com/s/4auvcjz3te4pic3/Luvilukuny_GS_vars.csv?dl=0", "./GID/Luvilukuny_GS_vars.csv", mode="wb")
geosv <- read.table(paste(dat_dir, "/Luvilukuny_GS_vars.csv", sep=""), header=T, sep=",")

#+ Data setup --------------------------------------------------------------
# Project test data to grid CRS
geosv.proj <- as.data.frame(project(cbind(geosv$Lon, geosv$Lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(geosv.proj) <- c("x","y")
geosv <- cbind(geosv, geosv.proj)

# Generate AfSIS grid cell ID's (GID)
res.pixel <- 1000
xgid <- ceiling(abs(geosv$x)/res.pixel)
ygid <- ceiling(abs(geosv$y)/res.pixel)
gidx <- ifelse(geosv$x<0, paste("W", xgid, sep=""), paste("E", xgid, sep=""))
gidy <- ifelse(geosv$y<0, paste("S", ygid, sep=""), paste("N", ygid, sep=""))
GID <- paste(gidx, gidy, geosv$L0, sep="-")
waypts <- cbind(geosv, GID)

