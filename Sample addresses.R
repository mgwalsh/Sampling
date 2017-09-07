# TZ what3words soil sample address geocoding
# W.Wu, J.Chen & M.Walsh, September 2017

# obtain a what3words API key @ https://accounts.what3words.com
# install.packages(c("downloader","threewords"), dependencies=TRUE)
require(downloader)
require(threewords)

# Data downloads -----------------------------------------------------------
# set working directory
dir.create("TZ_w3w", showWarnings=F)
setwd("./TZ_w3w")

# download MobileSurvey data
download("https://www.dropbox.com/s/5yp23mlyhx8irp8/Soil_sample_2017_09_06.csv.zip?raw=1", "Soil_sample_2017_09_06.csv.zip", mode="wb")
unzip("Soil_sample_2017_09_06.csv.zip", overwrite=T)
samp <- read.table("Soil_sample_2017_09_06.csv", header=T, sep=",")

# Data setup --------------------------------------------------------------
# convert lat/lon to numeric and restrict to samples located south of the equator
samp$lat <- as.numeric(as.character(samp$lat))
samp$lon <- as.numeric(as.character(samp$lon))
samp <- samp[samp$lat < 0,]
samp <- na.omit(samp)

# What3Words --------------------------------------------------------------
# very slow for this big file but you can always <esc>
w3w <- matrix(NA, dim(samp)[1], 3)
for(i in 1:dim(samp)[1]){ 
  w3w[i,] <- from_position(key = "TE2QLEOP", positions = c(samp[i,"lat"], samp[i,"lon"]))$words
  print(w3w[i,])
}
colnames(w3w) <- c("w1","w2","w3")

# Write output file -------------------------------------------------------
samp <- cbind(samp, w3w)
write.csv(samp, "Soil_sample_w3w.csv", row.names = F)
