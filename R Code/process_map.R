

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


###### Load map.list
map.list.df <- read.csv(paste(input.folder, "/mapindex/", sub(" ", "", region), "_maplist.csv", sep=""), stringsAsFactors = FALSE)
map.list <- map.list.df$name[map.list.df$process]


######  Iterate through maps and process them
for (m in 1:length(map.list)) {

  map.name <- map.list[m]
  
  ###### Load entire map
  bil.path <- list.files(path = file.path(map.collection.folder, map.name), pattern = '-rect.bil$', full.names = TRUE)
  full.map <- stack(bil.path)
  
  ###### Partition map
  source("R Code/partition_images.R")
  map.part.list <- partitionImage(full.map, (ceiling(nrow(full.map)/3)), 0)
  map.part.df <- getPartitionInfo(map.part.list)
  
  ###### Save partitions as .grd file and .jpg image
  source("R Code/save_files.R")
  
  foldername.vec <- vector("character", length(map.part.list))
  for (p in 1:length(map.part.list)) {
  	foldername <- file.path(output.collection.folder, map.name, paste0("partition_", p))
  	foldername.vec[p] <- foldername
  	filename <- paste(map.name, "_", p, sep="")
  	saveFile(map.part.list[[p]], filename, foldername)
  	jpeg(filename = file.path(foldername, paste0(filename, ".jpg")), 
  	     width = ncol(map.part.list[[p]]), 
  	     height = nrow(map.part.list[[p]]), 
  	     units = "px", pointsize = 12, quality = 100)
  	plotRGB(map.part.list[[p]])
  	dev.off()
  }
  
  ###### Load and proces map partitions (multi core)
  source("R Code/process_partition.R")
  
  # Register multicore
  select.partitions <- 1:length(map.part.list)
  if (OS == "Win") {
    cl <- makeCluster(cores)
    registerDoSNOW(cl)
  } else {
    registerDoMC(cores)
  }
  
  # Iterate over partitions & process in parallel
  foreach(i=1:length(select.partitions)) %dopar% {
    
    p <- select.partitions[i]
    output.folder <- paste(output.collection.folder, "/", map.name, "/", "partition_", p, sep="")
    partition.name <- paste(map.name, "_", p, sep="")
    
    library(raster)
    partition.map <- stack(paste(output.folder, "/", partition.name, ".grd", sep=""))
    partition.map[is.na(partition.map)] <- 255
    
    processPartition(partition.map, output.folder, partition.name, input.folder)
    print("Done.")
  } 
  
  
  ##### Combine vectorized partitions and make network
  source("R Code/combine_partitions.R")
  map.combined <- combinePartitions(select.partitions, output.collection.folder, map.name)
  map.sldf <- map.combined[[1]]
  map.raster <- map.combined[[2]]
  output.folder <- paste(output.collection.folder, "/", map.name, sep="")
  makeNetwork(inputlines.sldf=map.sldf, inputraster=map.raster, output.folder, input.folder, map.name, OS=OS, cores=cores)

}
