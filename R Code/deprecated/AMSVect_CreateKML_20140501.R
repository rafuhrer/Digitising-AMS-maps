

library(rgdal)
library(rgeos)
library(maptools)

folder <- "C:/Users/Philipp/Projects/HTND Local/AMS Road Vectorization/Output/South Asia/Combined"
name <- "combined"
roads <- readOGR(folder, name)

writeOGR(roads, dsn=paste(folder, "/roads.kml", sep=""), layer= "roads", driver="KML")


mainroads <- roads[roads$newsp == 1,]
otherroads <- roads[roads$newsp == 0,]

writeOGR(mainroads, dsn=paste(folder, "/mainroads.kml", sep=""), layer= "mainroads", driver="KML")
writeOGR(otherroads, dsn=paste(folder, "/otherroads.kml", sep=""), layer= "otherroads", driver="KML")