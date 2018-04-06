###### Set working machine
machine = "cederman1"

###### Set region to be processed
region = "West Africa"

###### Set machine specifications
if (machine == "ASUS") {
  setwd("D:/Projects/HTND")
  map.collection.folder <- paste("D:/Projects/HTND Local/AMS Road Vectorization/Maps/", region, sep="")
  output.collection.folder <- paste("D:/Projects/HTND Local/AMS Road Vectorization/Output/", region, sep="")
  input.folder <- "D:/Projects/HTND/"
  OS <- "Win"
  cores = 4
  library(doSNOW) 
} else if (machine == "cederman1") {
  setwd("/icr/home/hunzikp/Projects/HTND/HTND-R/AMS Road Vectorization")
  map.collection.folder <- "/icr/home/hunzikp/Projects/HTND/Maps"
  output.collection.folder <- paste("/icr/home/hunzikp/Projects/HTND/Output/", region, sep="")
  input.folder <- "/icr/home/hunzikp/Projects/HTND/HTND-R/AMS Road Vectorization/"
  OS <- "UNIX"
  cores = 8
  library(doMC)
} else if (machine == "ivt") {
  setwd("/Network/Servers/kosrae.ethz.ch/Volumes/ivt-home/hunzikp/AMS Road Vectorization")
  map.collection.folder <- "/Network/Servers/kosrae.ethz.ch/Volumes/ivt-home/hunzikp/Maps"
  output.collection.folder <- "/Network/Servers/kosrae.ethz.ch/Volumes/ivt-home/hunzikp/Output"
  input.folder <- "/Network/Servers/kosrae.ethz.ch/Volumes/ivt-home/hunzikp/AMS Road Vectorization/"
  OS <- "UNIX"
  cores = 12
  library(doMC)
} 


###### Load libraries
library(rgdal)
library(raster)
library(foreach)



###### Load map.list
map.list.df <- read.csv(paste(input.folder, "Input/MapIndex/", sub(" ", "", region), "_maplist.csv", sep=""), stringsAsFactors = FALSE)
map.list.df <- map.list.df[map.list.df$process,]
map.list <- map.list.df$name[map.list.df$process]


###### Create a data frame with all neighborhood dyads
letters.df <- data.frame(lat=LETTERS, latnum=1:26)
map.list.df <- merge(map.list.df, letters.df, all.x=T, all.y=F, by="lat", sort=FALSE)
map.list.df$mapid <- 1:nrow(map.list.df)

mn.list <- vector("list", nrow(map.list.df))
for (m in 1:length(mn.list)) {
  this.name <- map.list.df$name[m]
  this.mapid <- map.list.df$mapid[m]
  this.lat <- map.list.df$latnum[m]
  this.lon <- map.list.df$lon[m]
  n1.mapid <- map.list.df$mapid[map.list.df$latnum == this.lat & map.list.df$lon == this.lon+1]
  n2.mapid <- map.list.df$mapid[map.list.df$latnum == this.lat & map.list.df$lon == this.lon-1]
  n3.mapid <- map.list.df$mapid[map.list.df$latnum == this.lat+1 & map.list.df$lon == this.lon]
  n4.mapid <- map.list.df$mapid[map.list.df$latnum == this.lat-1 & map.list.df$lon == this.lon]
  n.mapid <- c(n1.mapid, n2.mapid, n3.mapid, n4.mapid)
  n.mapid <- n.mapid[!is.na(n.mapid)]
  n.mapid <- n.mapid[n.mapid < this.mapid]
  if (length(n.mapid) == 0) {
    n.mapid <- NA
  }
  n.df <- data.frame(mapid=this.mapid, nbid=n.mapid, stringsAsFactors=FALSE)
  mn.list[[m]] <- n.df
}
mn.df <- do.call("rbind", mn.list)
mn.df <- merge(mn.df, map.list.df[,c("mapid", "name")], by="mapid", all.x=TRUE, all.y=FALSE, sort=FALSE)
mn.df <- merge(mn.df, map.list.df[,c("mapid", "name")], by.x="nbid", by.y="mapid", all.x=TRUE, all.y=FALSE, sort=FALSE)
names(mn.df) <- c("mapid", "mapid2", "name2", "name")
mn.df <- na.omit(mn.df)
mn.df <- mn.df[!(mn.df$name == mn.df$name2),]




###### Create snaplines for each dyad and break nodes in original data set

### Load PostGIS DB handler functions and prepare schema
source(paste(input.folder, "R Code/AMSVect_PGHandler_20140425.R", sep=""))
con <- getPGConn("growup", "5432", "cederman.ethz.ch", "admin", "hNo7Yoo")
schemaname <- "mapcombine"
dbCreateSchema(con, schemaname)

### Load functions
source(paste(input.folder, "R Code/AMSVect_MergeLines_20140424.R", sep=""))
source(paste(input.folder, "R Code/AMSVect_NetworkTools_20140423.R", sep=""))
source(paste(input.folder, "R Code/AMSVect_trace_20140427.R", sep=""))
source(paste(input.folder, "R Code/AMSVect_CombinePartitions_20140426.R", sep=""))
source(paste(input.folder, "R Code/AMSVect_SaveFiles_20130604.R", sep=""))
source(paste(input.folder, "R Code/AMSVect_BreakNodes2_20130529.R", sep=""))

### Iterate through dyads
output.folder <- paste(output.collection.folder, "/Combined", sep="")
for (d in 1:nrow(mn.df)) {
  
  map1.name <- mn.df$name[d]
  map2.name <- mn.df$name2[d]
  
  # Load maps and copy maps to Combined folder (if they aren't there yet)
  map1.path <- paste(output.folder, "/", map1.name, ".shp", sep="")
  if (!file.exists(map1.path)) {
    map1.sldf <- readOGR(paste(output.collection.folder, "/", map1.name, sep=""), "final")
    saveFile(map1.sldf, map1.name, output.folder)
  } else {
    map1.sldf <- readOGR(output.folder, map1.name)
  }
  map2.path <- paste(output.folder, "/", map2.name, ".shp", sep="")
  if (!file.exists(map2.path)) {
    map2.sldf <- readOGR(paste(output.collection.folder, "/", map2.name, sep=""), "final")
    saveFile(map2.sldf, map2.name, output.folder)
  } else {
    map2.sldf <- readOGR(output.folder, map2.name)
  }
  
  # Format SLDFs
  map1.sldf <- assignLineID(map1.sldf, 1)
  map2.sldf <- assignLineID(map2.sldf, 2)
  
  map1.sldf$partition <- 1
  map2.sldf$partition <- 2
  
  # Create polygon of intersection region between maps
  map1.env <- gEnvelope(map1.sldf)
  map2.env <- gEnvelope(map2.sldf)
  map1.height <- bbox(map1.env)[2,2] - bbox(map1.env)[2,1]
  map1.buf <- gBuffer(map1.env, width=map1.height/8)
  map2.height <- bbox(map2.env)[2,2] - bbox(map2.env)[2,1]
  map2.buf <- gBuffer(map2.env, width=map2.height/8)
  map.int <- gIntersection(map1.buf, map2.buf)
  
  # Extract lines in intersection region
  combo.sldf <- mergeSLDF(map1.sldf, map2.sldf)
  intrs <- which(gIntersects(map.int, combo.sldf, byid=TRUE))
  if (length(intrs) > 0) {
    intersection.sldf <- combo.sldf[intrs,]
    
    # Create snaplines
    snaplines.sldf <- snapNetwork(con, schemaname, inputlines=intersection.sldf, buffersize=0.1, breakfirst=FALSE, snapAnywhere=TRUE, partitionName="partition", parallel=FALSE)
    any.newlines <- any(!(snaplines.sldf$lid %in% intersection.sldf$lid))
    if (any.newlines) {
      newlines.sldf <- snaplines.sldf[which(!(snaplines.sldf$lid %in% intersection.sldf$lid)),]
      newlines.sldf$statepred <- 0
      newlines.sldf$stateprob <- 0
      newlines.sldf$newsp <- 0
      
      # Save new lines
      savelines.sldf <- newlines.sldf
      savelines.df <- savelines.sldf@data
      savelines.df <- savelines.df[,c("lid", "stateprob", "statepred", "newsp")]
      savelines.sldf@data <- savelines.df
      filename <- paste(map1.name, "_", map2.name, sep="")
      saveFile(savelines.sldf, filename, output.folder)
      
      # Break lines in maps and replace according files in output folder
      names <- c(map1.name, map2.name)
      maps <- list(map1.sldf, map2.sldf)
      for (i in 1:2) {
        
        this.map <- maps[[i]]
        this.name <- names[i]
        
        mapc.sldf <- mergeSLDF(this.map, newlines.sldf)
        mapc.sldf <- breakNodes(con, schemaname, mapc.sldf, idcol="lid", verbose=FALSE, original.lid=TRUE)
        mapc.sldf$lid <- as.character(mapc.sldf$lid)
        mapc.sldf$lid_original[is.na(mapc.sldf$lid_original)] <- mapc.sldf$lid[is.na(mapc.sldf$lid_original)]
        mapc.sldf <- mapc.sldf[which(mapc.sldf$lid_original %in% this.map$lid),]
        mapc.df <- mapc.sldf@data
        this.map.df <- (this.map@data)[,c("lid", "stateprob", "statepred", "newsp")]
        names(this.map.df) <- c("lid_original", "stateprob", "statepred", "newsp")
        mapc.df$order <- 1:nrow(mapc.df)
        mapc.df <- merge(mapc.df, this.map.df, by="lid_original", all.x=TRUE, all.y=FALSE, sort=FALSE)
        mapc.df <- mapc.df[order(mapc.df$order),]
        mapc.df <- mapc.df[,c("lid", "stateprob", "statepred", "newsp")]
        mapc.sldf@data <- mapc.df
        file.remove(paste(output.folder, "/", this.name, ".shp", sep=""))
        file.remove(paste(output.folder, "/", this.name, ".shx", sep=""))
        file.remove(paste(output.folder, "/", this.name, ".dbf", sep=""))
        
        saveFile(mapc.sldf, this.name, output.folder)
      }
    } else {
      print(paste("No snaplines for combination", map1.name, "-", map2.name, "!"))
      flush.console()
    }
  } else {
    print(paste("No snaplines!"))
    flush.console()
  }
}


### Iterate through all parts and combine into large data set

part.names <- list.files(output.folder, pattern=".shp")
for (m in 1:length(part.names)) {
  name <- part.names[m]
  name <- sub("^([^.]*).*", "\\1", name) 
  this.sldf <- readOGR(output.folder, name)
  this.sldf <- assignLineID(this.sldf, name)
  if (m == 1) {
    combined.sldf <- this.sldf
  } else {
    combined.sldf <- mergeSLDF(combined.sldf, this.sldf)
  }
}
saveFile(combined.sldf, "combined", output.folder)




### Connect Classified Segments
source(paste(input.folder, "R Code/AMSVect_ClassifyConnector_20140601.R", sep=""))
combined.sldf$statepred <- combined.sldf$newsp
combined.sldf <- combined.sldf[,names(combined.sldf) != "newsp"]
connected.sldf <- connectClassified(combined.sldf, inputraster=NULL, im.th=10, w=3, buffer=c(0.75), clustersize=c(0))
saveFile(connected.sldf, "combined2", output.folder)
