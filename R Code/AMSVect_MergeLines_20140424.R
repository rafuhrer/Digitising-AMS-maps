
require(igraph)
require(rgeos)


ml.graph.SLDF <- function(inputlines, plot=FALSE) {
  # Create a graph from SLDF (every line end in SLDF becomes vertex)
  
  # Store line attributes for later usage
  inputlines.df <- inputlines@data
  
  # Create vertex and edge list from inputlines SLDF
  lid.vec <- as.character(inputlines$lid)
  length.vec <- gLength(inputlines, byid=TRUE)
  start.coordinates.list <-  lapply(coordinates(as.SpatialLines.SLDF(inputlines)), function(x) (x[[1]])[1,])
  end.coordinates.list <-  lapply(coordinates(as.SpatialLines.SLDF(inputlines)), function(x) (x[[1]])[nrow((x[[1]])),])
  verteces.df <- data.frame(unique(rbind(do.call("rbind", start.coordinates.list), do.call("rbind", end.coordinates.list))))
  names(verteces.df) <- c("x", "y")
  verteces.df$pid <- 1:nrow(verteces.df)
  edge.df <- data.frame(pid=numeric(), pid2=numeric(), lid=character(), weight=numeric(), stringsAsFactors=FALSE)
  
  for (i in 1:nrow(inputlines)) {
    startx <- (start.coordinates.list[[i]])[1]
    starty <- (start.coordinates.list[[i]])[2]
    endx <- (end.coordinates.list[[i]])[1]
    endy <- (end.coordinates.list[[i]])[2]
    pid <- verteces.df$pid[verteces.df$x == startx & verteces.df$y == starty]
    pid2 <- verteces.df$pid[verteces.df$x == endx & verteces.df$y == endy]
    lid <- lid.vec[i]
    weight <- length.vec[i]
    edge.df[i,1:2] <- c(pid, pid2)
    edge.df[i,3] <- lid
    edge.df[i,4] <- weight
  }
  edge.df$eid <- 1:nrow(edge.df)
  
  # Create graph
  graph <- graph.data.frame(edge.df, directed=FALSE)
  
  # Add line coordinates as vertex attributes
  graph.verteces.df <- data.frame(pid=as.numeric(V(graph)$name))
  graph.verteces.df <- merge(graph.verteces.df, verteces.df, by="pid", all=TRUE, sort=FALSE)
  graph <- set.vertex.attribute(graph, name="x", value=graph.verteces.df$x)
  graph <- set.vertex.attribute(graph, name="y", value=graph.verteces.df$y)
  
  if(plot) {
    plot(graph, layout=layout.sldf(graph), vertex.size=1, vertex.label=NA, vertex.color="red",
         edge.color="black")
  }
  
  return(graph)
}


ml.mergeSLDF <- function(df1, df2) {
  merge.df <- rbind(df1@data, df2@data)
  l1.ls <- df1@lines
  l2.ls <- df2@lines
  merge.ls <- c(l1.ls, l2.ls)
  merge.sl <- SpatialLines(merge.ls)
  merge.sldf <- SpatialLinesDataFrame(merge.sl, merge.df, match.ID = FALSE)
  return(merge.sldf)
}



mergeLines <- function(inputlines, verbose=FALSE) {

  baselines <- inputlines
  baselines@data <- data.frame(lid=baselines$lid)
  baselines$lid <- as.character(baselines$lid)
  basegraph <- ml.graph.SLDF(inputlines)
  c <- 0
  repeat {
    v.degree <- degree(basegraph, V(basegraph))
    d2.v <- V(basegraph)[v.degree==2]
    if(length(d2.v) == 0) {
      break
    }
    d2.e <- E(basegraph)[adj(d2.v)]
    d2.e.lid <- d2.e$lid
    candidate.lids <- unique(d2.e.lid)
    
    removelines.lids <- vector("character", 0)
    newlines.ls <- vector("list", 0)
    newlids.vec <- vector("character", 0)
    circular.n <- 0
    for (i in 1:length(d2.v)) {
      this.v.name <- d2.v$name[i]
      this.v <- d2.v[d2.v$name == this.v.name]
      this.e <- E(basegraph)[adj(this.v)]
      this.e.lid <- this.e$lid
    
      if (length(this.e.lid) == 1) {
        # any vertex with degree=2 but only one distinct edge is a loop
        circular.n <- circular.n + 1
      } else {
        if (all(this.e.lid %in% candidate.lids)) {
          this.line <- ((baselines[baselines$lid == this.e.lid[1],])@lines)[[1]]
          this.line.ID <- (((baselines[baselines$lid == this.e.lid[1],])@lines)[[1]])@ID
          next.line <- ((baselines[baselines$lid == this.e.lid[2],])@lines)[[1]]
          merge.sl <- SpatialLines(list(this.line, next.line))
          new.line <- gLineMerge(merge.sl)
          new.lines <- Lines((((new.line@lines)[[1]])@Lines), this.line.ID)
          newlines.ls[[length(newlines.ls) + 1]] <- new.lines
          newlids.vec[length(newlids.vec) + 1] <- this.e.lid[1]
          candidate.lids <- candidate.lids[!(candidate.lids %in% this.e.lid)]
          removelines.lids[length(removelines.lids)+1] <- this.e.lid[1]
          removelines.lids[length(removelines.lids)+1] <- this.e.lid[2]
        }
      }
    }
    
    if (circular.n == length(d2.v)) {
      # In case only loops are left
      break
    }
    
    newlines.df <- data.frame(lid=newlids.vec)
    newlines.df$lid <- as.character(newlines.df$lid)
    newlines.sl <- SpatialLines(newlines.ls)
    newlines.sldf <- SpatialLinesDataFrame(newlines.sl, newlines.df, match.ID = FALSE)
    if (all(baselines$lid %in% removelines.lids)) {
      baselines <- newlines.sldf
    } else {
      baselines <- baselines[!(baselines$lid %in% removelines.lids),]
      baselines <- ml.mergeSLDF(baselines, newlines.sldf)
    }
    basegraph <- ml.graph.SLDF(baselines)
    
    c <- c + 1
    if (verbose) {
      print(paste("Completed round ", c, "...", sep=""))
      flush.console()
    }
  }
  
  return(baselines)
}



mergeLinesDB <- function(con, schemaname, inputlines, verbose=FALSE, idcol="lid") {
  
  # LINE MERGING ALGORITHM FOR LARGE DATA SETS
  #
  # Performs line merging on PostGIS DB; Faster than local network-based line merging algorithm for data sets with
  # large number of broken nodes.
  #
  # Last Edited:
  # 27/04/2014
  
  # Remove everything but the id column from the inputline data frame
  data.df <- data.frame((inputlines@data)[,which(names(inputlines@data) == idcol)])
  names(data.df) <- "lid"
  inputlines@data <- data.df
  
  # Upload SLDF to DB
  if (verbose) {
    print(paste("Writing", length(inputlines), "lines on DB..."))
    flush.console()
  }
  r <- dbWriteSpatial(con, inputlines, schemaname, "inputlines", T)
  
  # Execute SQL script on DB that creates a table with the merged lines
  if (verbose) {
    print(paste("Compiling merge nodes on DB..."))
    flush.console()
  }
  rs <- dbRunScript(con, path='SQL Code/MergeLines_20140427.sql', script.param=list(SCHEMANAME=schemaname))
  
  # Get table with merged lines
  outputlines <- dbReadSpatial(con, schemaname, tablename="outputlines", idcol="lid")
  
  # Clean up on DB
  if (verbose) {
    print(paste("Cleaning up DB..."))
  }
  rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".outputlines;", sep=""))
  
  if (verbose) {
    print(paste("Done."))
  }
  return(outputlines)
}




