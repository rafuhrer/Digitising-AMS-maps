
###### Set working directory
setwd("C:/Users/spadmin/workspace/HTND-R/")

###### Load libraries
library(raster)
library(sp)
library(rgdal)
library(RPostgreSQL)
library(rgeos)

###### Import sample of AMS Test Map
map.name <- "1301-NB32-Douala_MiniSample"
partition.map <- stack(paste("Test Map/1301-NB32-Douala_MiniSample/", map.name, ".jpg", sep=""))
plot(partition.map)
foldername <- "C:/Users/spadmin/workspace/HTND-R/Test Map/1301-NB32-Douala_MiniSample/"