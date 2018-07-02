###### Set working machine
machine = "gdip"

###### Set region to be processed
region = "Europe"

###### Set machine specifications
if (machine == "pakin") {
  setwd("")
  map.collection.folder <- paste("", region, sep="")
  output.collection.folder <- paste("", region, sep="")
  input.folder <- ""
  OS <- "Win"
  cores = 8
  library(doSNOW) 
} else if (machine == "hunzikp") {
  setwd("/home/hunzikp/Projects/amsvec_europe/Digitising-AMS-maps")
  map.collection.folder <- "/home/hunzikp/Projects/amsvec_europe/maps"
  output.collection.folder <- "/home/hunzikp/Projects/amsvec_europe/output"
  input.folder <- "/home/hunzikp/Projects/amsvec_europe/input"
  OS <- "UNIX"
  cores = 6
  library(doMC)
} else if (machine == "gdip") {
  setwd("/home/philipp/Projects/amsvec_europe/Digitising-AMS-maps")
  map.collection.folder <- "/home/philipp/Projects/amsvec_europe/maps"
  output.collection.folder <- "/home/philipp/Projects/amsvec_europe/output"
  input.folder <- "/home/philipp/Projects/amsvec_europe/input"
  OS <- "UNIX"
  cores = 10
  library(doMC)
}


###### Load libraries
library(rgdal)
library(raster)
library(foreach)
library(velox)


###### Load map.list
map.list.df <- read.csv(paste(input.folder, "/mapindex/", sub(" ", "", region), "_maplist.csv", sep=""), stringsAsFactors = FALSE)
map.list.df <- map.list.df[map.list.df$process,]
map.list <- map.list.df$name[map.list.df$process]

######   Create small versions of selected maps, just for illustration
# Create new directory
if (dir.exists(file.path(output.collection.folder, "mini"))) {
  unlink(file.path(output.collection.folder, "mini"), recursive = TRUE)
}
dir.create(file.path(output.collection.folder, "mini"))

# Create small maps
xlim <- map.list.df$xlim
for (m in 1:length(map.list)) {
  
  map.name <- map.list[m]
  
  ## Load entire map
  bil.path <- list.files(path = file.path(map.collection.folder, map.name), pattern = '-rect.bil$', full.names = TRUE)
  full.map <- stack(bil.path)
  full.vx <- velox(full.map)
  
  ## Crop (if necessary)
  if (xlim[m] != "") {
    xl <- as.numeric(strsplit(xlim[m], split = ", ", fixed = TRUE)[[1]])
    extent <- full.vx$extent
    if (is.finite(xl[1])) {
      extent[1] <- xl[1]
    }
    if (is.finite(xl[2])) {
      extent[2] <- xl[2]
    }
    full.vx$crop(x = extent)
  }
  
  ## Aggregate and save
  full.vx$aggregate(factor = 16, aggtype = 'mean')
  full.vx$write(path = file.path(output.collection.folder, "mini", paste0(map.name, ".tif")), overwrite = TRUE)
  
  print(paste(m, length(map.list)))
  flush.console()
}

###### Create a data frame with all neighborhood dyads
map.list.df$mapid <- 1:nrow(map.list.df)

mn.df <- data.frame(mapid = numeric(), mapid2 = numeric(), name2 = character(), name = character())
for (i in 1:nrow(map.list.df)) {
  
  if (map.list.df$nright[i] != "") {
    new.df <- data.frame(mapid = map.list.df$mapid[i])
    mapid2 <- map.list.df$mapid[map.list.df$name == map.list.df$nright[i]]
    name2 <- map.list.df$name[map.list.df$mapid == mapid2]
    new.df$mapid2 <- mapid2
    new.df$name2 <- name2
    new.df$name <- map.list.df$name[i]
    mn.df <- rbind(mn.df, new.df)
  }
  
  if (map.list.df$ntop[i] != "") {
    new.df <- data.frame(mapid = map.list.df$mapid[i])
    mapid2 <- map.list.df$mapid[map.list.df$name == map.list.df$ntop[i]]
    name2 <- map.list.df$name[map.list.df$mapid == mapid2]
    new.df$mapid2 <- mapid2
    new.df$name2 <- name2
    new.df$name <- map.list.df$name[i]
    mn.df <- rbind(mn.df, new.df)
  }
}


###### Create snaplines for each dyad and break nodes in original data set

### Load PostGIS DB handler functions and prepare schema
source("R Code/pg_handler.R")
# con <- getPGConn("growup", "5432", "cederman.ethz.ch", "admin", "hNo7Yoo")
con <- getPGConn("amsvec", "5432", "icr-s02.ethz.ch", 'admin', 'hNo7Yoo')
schemaname <- "mapcombine"
dbCreateSchema(con, schemaname)

### Load functions
source("R Code/merge_lines.R")
source("R Code/network_tools.R")
source("R Code/trace.R")
source("R Code/combine_partitions.R")
source("R Code/save_files.R")
source("R Code/break_nodes.R")

### Iterate through dyads
output.folder <- paste(output.collection.folder, "/Combined", sep="")
for (d in 1:nrow(mn.df)) {
  
  map1.name <- mn.df$name[d]
  map2.name <- mn.df$name2[d]
  
  # Load and crop map 1
  map1.sldf <- readOGR(paste(output.collection.folder, "/", map1.name, sep=""), "final")
  xlim <- map.list.df$xlim[map.list.df$name == mn.df$name[d]]
  if (xlim != "") {
    xl <- as.numeric(strsplit(xlim, split = ", ", fixed = TRUE)[[1]])
    extent <- extent(gEnvelope(map1.sldf))
    if (is.finite(xl[1])) {
      extent[1] <- xl[1]
    }
    if (is.finite(xl[2])) {
      extent[2] <- xl[2]
    }
    map1.sldf <- crop(map1.sldf, extent)
  }
  
  # Load and crop map 2
  map2.sldf <- readOGR(paste(output.collection.folder, "/", map2.name, sep=""), "final")
  xlim <- map.list.df$xlim[map.list.df$name == mn.df$name2[d]]
  if (xlim != "") {
    xl <- as.numeric(strsplit(xlim, split = ", ", fixed = TRUE)[[1]])
    extent <- extent(gEnvelope(map2.sldf))
    if (is.finite(xl[1])) {
      extent[1] <- xl[1]
    }
    if (is.finite(xl[2])) {
      extent[2] <- xl[2]
    }
    map2.sldf <- crop(map2.sldf, extent)
  }
  
  # Save maps (if they aren't there yet)
  map1.path <- paste(output.folder, "/", map1.name, ".shp", sep="")
  if (!file.exists(map1.path)) {
    saveFile(map1.sldf, map1.name, output.folder)
  }
  map2.path <- paste(output.folder, "/", map2.name, ".shp", sep="")
  if (!file.exists(map2.path)) {
    saveFile(map2.sldf, map2.name, output.folder)
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
  print(paste("Processed", d, "of", nrow(mn.df)))
  flush.console()
}

## Add maps that weren't in any dyad to combinations directory
remaining.df <- map.list.df[!(map.list.df$name %in% mn.df$name),]
remaining.df <- remaining.df[!(remaining.df$name %in% mn.df$name2),]
if (nrow(remaining.df) > 0) {
  
  for (i in 1:nrow(remaining.df)) {
    
    map.name <- remaining.df$name[i]
    map.sldf <- readOGR(paste(output.collection.folder, "/", map.name, sep=""), "final")
    map.path <- paste(output.folder, "/", map.name, ".shp", sep="")
    if (!file.exists(map.path)) {
      saveFile(map.sldf, map.name, output.folder)
    }
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
  print(paste(m, "of", length(part.names)))
}
saveFile(combined.sldf, "combined", output.folder)




### Connect Classified Segments
# source("R Code/classify_connector.R")
# combined.sldf$statepred <- combined.sldf$newsp
# combined.sldf <- combined.sldf[,names(combined.sldf) != "newsp"]
# connected.sldf <- connectClassified(combined.sldf, inputraster=NULL, im.th=10, w=3, buffer=c(0.75), clustersize=c(0))
# saveFile(connected.sldf, "combined2", output.folder)
