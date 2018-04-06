

##### Combine partition cleanlines and classifed raster images
combinePartitions <- function(select.partitions, output.collection.folder, map.name) {

  map.sldf <- NULL
  map.raster <- NULL
  for (p in select.partitions) {
    output.folder <- paste(output.collection.folder, "/", map.name, "/", "partition_", p, sep="")
    noroads.path <- paste(output.folder, "/noroads.txt", sep="")
    if (file.exists(noroads.path)) {
      print(paste("No roads in partition", p))
      flush.console()
    } else {
      partition.name <- paste(map.name, "_", p, sep="")
      partition.sldf <- readOGR(output.folder, "sslines")
      partition.sldf$partition <- p
      partition.sldf <- assignLineID(partition.sldf, p)
      partition.raster <- raster(paste(output.folder, "/red.asc", sep=""))
      if (is.null(map.sldf)) {
        map.sldf <- partition.sldf
        map.raster <- partition.raster
      } else {
        map.sldf <- mergeSLDF(map.sldf, partition.sldf)
        map.raster <- merge(map.raster, partition.raster)
      }
    }
  }
  
  return(list(map.sldf, map.raster))
}


##### Reassignes Lines-IDs in SLDF, either adds "pi" or "i_", depending on whether i is numeric or string
assignLineID <- function(inputlines, i) {
  
  orig.id <- unlist(lapply(inputlines@lines, function (x) slot(x, "ID")))
  if (is.numeric(i)) {
    new.id <- paste("p", i, "_", orig.id, sep="")
  } else {
    new.id <- paste(i, "_", orig.id, sep="")
  }
  outputlines <- spChFIDs(inputlines, new.id)
  outputlines$lid <- new.id
  return(outputlines)
}


##### Appends two SLDFs
mergeSLDF <- function(df1, df2) {
  merge.df <- rbind(df1@data, df2@data)
  l1.ls <- df1@lines
  l2.ls <- df2@lines
  merge.ls <- c(l1.ls, l2.ls)
  merge.sl <- SpatialLines(merge.ls)
  merge.sldf <- SpatialLinesDataFrame(merge.sl, merge.df, match.ID = FALSE)
  return(merge.sldf)
}