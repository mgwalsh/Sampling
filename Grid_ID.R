#' Generate grid cell ID's (GID's) for field navigation
#' J. Chen & M. Walsh, August 2015
 
#+ Required packages
# install.packages(c("downloader","raster","rgdal")), dependencies=TRUE)
require(downloader)
require(rgdal)

#+ Data download -----------------------------------------------------------
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

# Generate AfSIS GID's
res.pixel <- 1000
xgid <- ceiling(abs(geosv$x)/res.pixel)
ygid <- ceiling(abs(geosv$y)/res.pixel)
gidx <- ifelse(geosv$x<0, paste("W", xgid, sep=""), paste("E", xgid, sep=""))
gidy <- ifelse(geosv$y<0, paste("S", ygid, sep=""), paste("N", ygid, sep=""))
GID <- paste(gidx, gidy, geosv$L0, sep="-")
geosv.gid <- cbind(geosv, GID)

#+ Write GPX file … requires GPSBabel to be installed ----------------------
# you can download GPSBabel at: http://www.gpsbabel.org/download.html
wpts <- geosv.gid[c(1:2,13)]
write.csv(wpts, "Waypoints.csv", row.names=FALSE)
# note that this system call has to point to the location of your GPSBabel application
system("/Applications/GPSBabelFE.app/Contents/MacOS/gpsbabel -i csv -f Waypoints.csv -o gpx -F Waypoints.gpx")

# if you have Garmin device plugged in, you can also write the resulting GPX file directly with:
# system("cp Waypoints.gpx /Volumes/GARMIN/Garmin/GPX")
