

###### Set working machine
machine = "ASUS"

###### Set region to be processed
region = "Asia Pacific"

###### Set machine specifications
if (machine == "ASUS") {
  setwd("D:/Projects/HTND")
  map.collection.folder <- paste("D:/Projects/HTND Local/AMS Road Vectorization/Maps/", region, sep="")
  output.collection.folder <- paste("D:/Projects/HTND Local/AMS Road Vectorization/Output/", region, sep="")
  input.folder <- "D:/Projects/HTND/"
  OS <- "Win"
  cores = 2
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
  output.collection.folder <- paste("/Network/Servers/kosrae.ethz.ch/Volumes/ivt-home/hunzikp/Output/", region, sep="")
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


######  Iterate through maps and process them
for (m in 1:length(map.list)) {

  map.name <- map.list[m]
  
  ###### Load entire map
  bil.path <- paste(map.collection.folder, "/", map.name, "/", map.name, "-WGS84.bil", sep="")
  if (file.exists(bil.path)) {
    full.map <- stack(bil.path)
  } else {
    tiff.path <- paste(map.collection.folder, "/", map.name, "/", map.name, "-WGS84.tiff", sep="")
    full.map <- stack(tiff.path)
  }
  
  ###### Partition map
  source("R Code/AMSVect_ImagePartitioner_20140421.R")
  map.part.list <- partitionImage(full.map, (ceiling(nrow(full.map)/3)), 0)
  map.part.df <- getPartitionInfo(map.part.list)
  
  
  ###### Save partitions as .grd file and .jpg image
  source("R Code/AMSVect_SaveFiles_20130604.R")
  
  foldername.vec <- vector("character", length(map.part.list))
  for (p in 1:length(map.part.list)) {
  	foldername <- paste(output.collection.folder, "/", map.name, "/", "partition_", p, sep="")
  	foldername.vec[p] <- foldername
  	filename <- paste(map.name, "_", p, sep="")
  	saveFile(map.part.list[[p]], filename, foldername)
  	jpeg(filename = paste(foldername, "/", filename, ".jpg", sep=""), width = ncol(map.part.list[[p]]), height = nrow(map.part.list[[p]]), units = "px", pointsize = 12, quality = 100)
  	plotRGB(map.part.list[[p]])
  	dev.off()
  }
  
  
  ###### Load and proces map partitions (multi core)
  source(paste(input.folder, "R Code/AMSVect_Main_20140507.R", sep=""))
  
  select.partitions <- c(1:length(map.part.list))
  if (OS == "Win") {
    cl <- makeCluster(cores)
    registerDoSNOW(cl)
  } else {
    registerDoMC(cores)
  }
  
  foreach(i=1:length(select.partitions)) %dopar% {
    p <- select.partitions[i]
    output.folder <- paste(output.collection.folder, "/", map.name, "/", "partition_", p, sep="")
    partition.name <- paste(map.name, "_", p, sep="")
    library(raster)
    partition.map <- stack(paste(output.folder, "/", partition.name, ".grd", sep=""))
    processPartition(partition.map, output.folder, partition.name, input.folder)
    print("Done.")
  } 
  
  
  ##### Combine vectorized partitions and make network
  source(paste(input.folder, "R Code/AMSVect_CombinePartitions_20140426.R", sep=""))
  map.combined <- combinePartitions(select.partitions, output.collection.folder, map.name)
  map.sldf <- map.combined[[1]]
  map.raster <- map.combined[[2]]
  output.folder <- paste(output.collection.folder, "/", map.name, sep="")
  makeNetwork(inputlines.sldf=map.sldf, inputraster=map.raster, output.folder, input.folder, map.name, OS=OS, cores=cores)

}
