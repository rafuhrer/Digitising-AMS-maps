

###### Set working machine
machine = "cederman1"

###### Set region to be processed
region = "Asia Pacific"

###### Set machine specifications
if (machine == "ASUS") {
  setwd("C:/Users/Philipp/Projects/HTND")
  map.collection.folder <- paste("C:/Users/Philipp/Projects/HTND Local/AMS Road Vectorization/Maps/", region, sep="")
  output.collection.folder <- paste("C:/Users/Philipp/Projects/HTND Local/AMS Road Vectorization/Output/", region, sep="")
  input.folder <- "C:/Users/Philipp/Projects/HTND/"
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
map.list <- map.list.df$name[map.list.df$process]


###### Prepare parallelization
if (OS == "Win") {
  cl <- makeCluster(cores)
  registerDoSNOW(cl)
} else {
  registerDoMC(cores)
}

######  Iterate through maps and process them
foreach(m=1:length(map.list)) %dopar% {
    
  source("R Code/AMSVect_SaveFiles_20130604.R")
  map.name <- map.list[m]
  
  ###### Load PostGIS DB handler functions and prepare schema
  source(paste(input.folder, "R Code/AMSVect_PGHandler_20140425.R", sep=""))
  con <- getPGConn("growup", "5432", "cederman.ethz.ch", "admin", "hNo7Yoo")
  schemaname <- tolower(gsub("-", "_", map.name))
  dbCreateSchema(con, schemaname)
  
  ##### Combine vectorized partitions
  source(paste(input.folder, "R Code/AMSVect_CombinePartitions_20140426.R", sep=""))
  output.folder <- paste(output.collection.folder, "/", map.name, sep="")
  partition.count <- sum(grepl("part", list.dirs(output.folder, full.names=FALSE, recursive=FALSE)))
  select.partitions <- c(1:partition.count)
  map.combined <- combinePartitions(select.partitions, output.collection.folder, map.name)
  map.raster <- map.combined[[2]]
  
  ##### Save "old" final lines (for safety...)
  final.sldf <- readOGR(output.folder, "final")
  saveFile(final.sldf, "finalold", output.folder)
  
  ##### Load simplified and perform road classification
  source(paste(input.folder, "R Code/AMSVect_ClassifyRoads3_20140505.R", sep=""))
  simplified.sldf <- readOGR(output.folder, "simplified")
  load(paste(input.folder, "Input/rClassifier.Rdata", sep=""))
  classified.sldf <- classifyRoads(con, schemaname, rClassifier, inputlines=simplified.sldf, inputpixels=map.raster, th=0.00001)
  saveFile(classified.sldf, "classified", output.folder)
  
  ##### Connect classified lines
  source(paste(input.folder, "R Code/AMSVect_ClassifyConnector_20140601.R", sep=""))
  connected.sldf <- connectClassified(classified.sldf, map.raster, im.th=10, w=3, buffer=c(250), clustersize=c(0))
  
  ##### Finalize and save lines
  final.sldf <- connected.sldf
  projection(final.sldf) <- projection(map.raster)
  saveFile(final.sldf, "final", output.folder)
  
  dbDropSchema(con, schemaname)
  rv <- dbDisconnect(con)
}





